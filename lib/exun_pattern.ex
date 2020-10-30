defmodule Exun.Pattern do
  import Exun.UI
  import Exun.Eq
  import Exun.Collect
  import Exun.Fun
  @uno {:numb, 1}
  @moduledoc """
  Match ASTs. Functions umatch and match try to match patter with a real expression. Rules for matching, by example:
  - umatch "f", "(any valid expression)"

   Will always match f => expression

  Ex: umatch "f","x^2+y"

  - umatch "f'x", "exp"

  Match f'x as exp AND f as $exp,x

  Ex: umatch "f'x", "-cos(x)"

  - umatch "f<op>g", "exp"

  Will tray to match f and g against exp using <op>=*,+,-,/,^ on any possible combination

  Ex: umatch "a+b", "2**x^2+3"

  - umatch "f**f'x", "exp"

  Will match if can find a product on exp of the form f ** d(f)/dx

  Ex: umatch "sin(x)*cos(x)"

  - umatch "f**$f,x"

  Will match if can find a product on exp of the form f ** integ(f,x)

  - umatch "f(x,y)", "exp"

  Will match if exp is a function call and can match x and y with its arguments.

  Ex: umatch "f(a,b)", "function(2**x,y^3)"

  - umatch "u**v'x","sin(x)*cos(x)", ["v**u'x"]

  Will match u and v against "sin(x)**cos(x)" and checks that condition "v**u'x" does not contains a symbolic integral. This
  case is used for integrate by parts: $u**v'x,x = u**v - $v**u'x,x

  """
  @doc """
  User function, try to match and prints
  """
  def umatch(taast, texpr, tconditions \\ [], transf \\ true) do
    los = match(taast, texpr, %{}, tconditions, transf)

    if los != [] do
      los
      |> Enum.each(fn {res, map} ->
        IO.puts("Match group #{res}")

        Enum.each(map, fn {name, value} ->
          # IO.inspect(name,label: "VarName")
          # IO.inspect(value, label: "VarValue")
          IO.puts("  #{tostr(name)}\t= #{tostr(value)}")
        end)
      end)
    else
      IO.puts("Cannot match")
    end
  end

  def match(taast, texpr, context, tconditions \\ [], transf \\ true) do
    {aast, _} = Exun.parse(taast)
    {expr, _} = Exun.parse(texpr, context)

    cond do
      aast |> elem(0) == :error ->
        {:error, {_, _, [msg, _]}} = aast
        throw("Pattern #{taast} : #{msg})")

      expr |> elem(0) == :error ->
        {:error, {_, _, [msg, _]}} = expr
        throw("Expression #{texpr} : #{msg})")

      true ->
        nil
    end

    conditions =
      Enum.map(tconditions, fn cnd ->
        {res, _} = Exun.parse(cnd)
        res
      end)

    match_ast(aast, expr, conditions, transf)
    |> remove_dups()
  end

  def remove_dups(los) do
    los
    # |> IO.inspect(label: "init")
    |> Enum.map(fn {:ok, sol} ->
      Enum.reduce(sol, %{}, fn {k, v}, nmap ->
        Map.put(nmap, k, coll(v))
      end)
    end)
    # |> IO.inspect(label: "collected")
    |> Enum.reduce(MapSet.new(), &MapSet.put(&2, &1))
    |> Enum.map(&{:ok, &1})
  end

  @doc """
  Make some expansions in east in order to match complex matching expressions.
  For example match "f*f'x" to "2*x^3" will match if it is transformed to
  "2*x*x^2" so f=x^2
  """
  def match_ast(aast, aexp, conditions \\ [], dotransforms \\ true) do
    aexp
    # |> IO.inspect(label: "expr")
    |> transform(dotransforms)
    # |> IO.inspect(label: "expr transformed")
    |> Enum.reduce([], fn expr, ac ->
      mnode(aast, expr, %{}) ++ ac
    end)
    |> Enum.reject(fn {res, map} ->
      res != :ok or map == %{}
    end)
    # Check pair definitions, if the map holds f and f'x
    # We must test if it is true
    |> check_def_consistency()
    # Check conditions passed by user
    |> Enum.reject(fn {res, map} ->
      res != :ok or map == %{}
    end)
    |> Enum.map(fn {:ok, map} ->
      res = if check_conds(map, conditions), do: :ok, else: :nocond
      {res, map}
    end)
  end

  defp check_def_consistency(maps) when is_list(maps) do
    maps
    |> Enum.map(&check_def_consistency(&1))
  end

  defp check_def_consistency(tmap) when is_tuple(tmap) do
    {:ok, map} = tmap

    res =
      Enum.reduce(map, :ok, fn {k, v}, state ->
        if state == :ko do
          :ko
        else
          # if k Is a derivate "f'x", check if exists f and test its derivative
          case k do
            {:deriv, {:vari, f}, {:vari, var}} ->
              case Map.fetch(map, {:vari, f}) do
                # No problem, user wants to identify f'x but not use f
                :error -> :ok
                {:ok, val} -> if eq(v, coll({:deriv, val, {:vari, var}})), do: :ok, else: :ko
              end

            _ ->
              :ok
          end
        end
      end)

    {res, map}
  end

  def transform(expr, exec) do
    if exec do
      [
        expr
        | case expr do
            {{:m, :mult}, lst} ->
              # Get list of posssible list for {:m,:mult}
              trx1(lst)
              |> Enum.map(fn l ->
                {{:m, :mult}, l}
              end)

            _ ->
              []
          end
      ]
    else
      [expr]
    end
  end

  @doc """
  trx1: {{:m,:mult},lst} where k={:elev, {:vari,a},{some}} in list
  will double matched, original and replacing k with a^(some-remain_lst)*a^(remain_lst)
  in a try to match f*f'x <- "2*x^3", so also will try "2*x^(3-2)*x^2" == "2*x*x^2" and then
  f==x^2
  """
  def trx1(lst) do
    # List of powers in list
    Enum.reduce(lst, [], fn el, ac ->
      case el do
        m1 = {:elev, _, _} -> [m1 | ac]
        _ -> ac
      end
    end)
    # for each transform lst
    |> Enum.map(fn power = {:elev, a, b} ->
      # IO.inspect(power,label: "power")
      remain = List.delete(lst, power)
      # |> IO.inspect(label: "remain")
      expon1 = coll({{:m, :mult}, remain |> Enum.sort(&smm(&1, &2))})
      # |> IO.inspect(label: "expon1")
      expon2 = coll(rest(b, expon1))
      # |> IO.inspect(label: "expon2")
      [coll({:elev, a, expon1}), coll({:elev, a, expon2}) | remain] |> Enum.sort(&smm(&1, &2))
      # |> IO.inspect(label: "trx1 result")
    end)
  end

  @doc """
  General matching function
  Try to match an abstract expresion agains real expression, for example
  abstract expression: u(x)*v(x)'x
  real expression: sin(x)^2

  match u(x) = sin(x) and v(x)'x= sin(x)
  so v(x)=-cos(x)

  u and v will be return in a map:
  %{"u(x)"=>"sin(x)", "v(x)"=>"-cos(x)"}

  return a list of tuples {:ok, matched_defs} or {:ko, matched_defs}
  matched_defs is a map that holds definitions
  """
  def mnode(aast, expr, map) do
    # IO.puts(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    # IO.inspect(aast, label: "aast = ")
    # IO.inspect(expr, label: "expr = ")
    # IO.inspect(map, label: "map = ")
    # IO.puts("..............................................")

    case {aast, expr} do
      # Two numbers must match exactly
      {{:numb, n}, {:numb, n}} ->
        [{:ok, map}]

      # Base and expon must match
      {{:elev, a, b}, {:elev, c, d}} ->
        mlist([a, b], [c, d], map)

      # Handle expon==1
      {{:elev, a, b}, expr} ->
        mlist([a, b], [expr, @uno], map)

      # Cannot isolate for now. Match sin(F) <= 1 would be possible
      # if we can isolate F=acos(1) and define F as constant, a number.
      {{:fcall, _name, _args}, {:numb, _}} ->
        [{:ko, map}]

      # Function def from abstract ast match if vars are used in expr
      {a = {:fcall, _name, _args}, expr} ->
        mfdef(a, expr, map)

      # see mderiv
      {der = {:deriv, {:vari, _}, {:vari, _}}, expr} ->
        mderiv(der, expr, map)

      # see minteg
      {integ = {:integ, {:vari, _}, _var}, expr} ->
        minteg(integ, expr, map)

      # Multiple sum or product. This can produce multiple tries for matching
      # We *must* check all of them,
      {a = {{:m, _op}, _l1}, expr} ->
        mmult(a, expr, map)

      # General match
      {{:vari, a}, expr} ->
        [checkmap(map, {:vari, a}, expr)]

      _ ->
        [{:ko, map}]
    end

    # |> IO.inspect(label: "MNode Ret<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n")
  end

  @doc """
  All possible matchings from l2 to l1. l2 must be equal or greater in lenght, if not
  there is no possible match. Caller must comlpete l2 with 'unity', this is not going
  to happend here. l1 is abstract, l2 a real expression.
    # if map holds any definition from l1, match it before generalize; if we can't, return ko
    # if yes, reduce l2 and l1 for that definitions. We have to substitute map values into l1
    # and then perform match
    # For now, try direct match; sin florituras
  """

  def mmult(aast = {{:m, op}, lsta}, east = {{:m, op}, lste}, mainmap) do
    # if more left matching elements than right, complete right with unity elements
    east =
      if length(lsta) > length(lste) do
        unity = if op == :suma, do: {:numb, 0}, else: {:numb, 1}

        {{:m, op}, lste ++ List.duplicate(unity, length(lsta) - length(lste))}
      else
        east
      end

    # IO.inspect(map, label: "mm Input map")
    # Explore all options for matching
    cmb = combin_expand(aast, east)
    # |> IO.inspect(label: "Order Expanded")
    Enum.reduce(cmb, [], fn set, acc ->
      # zip pattern and expresion and try to match with mnode with current defs map
      (
        zipped = List.zip([lsta, set])

        Enum.reduce(zipped, [{:ok, mainmap}], fn {abs, exp}, maplist ->
          if maplist == [] do
            [{:ko, mainmap}]
          else
            [{res, map}] = maplist
            # IO.puts(par_inspect({abs, exp}))

            cond do
              res == :ok and is_list(exp) and length(exp) > 1 ->
                mnode(abs, {{:m, op}, exp}, map)

              res == :ok and is_list(exp) ->
                [sub] = exp
                mnode(abs, sub, map)

              res == :ok ->
                mnode(abs, exp, map)

              true ->
                [{res, map}]
            end
          end
        end)

        # |> IO.inspect(label: "Zipped maps")
      ) ++ acc
    end)
  end

  def mmult(aast = {{:m, op}, _lsta}, expr, map) do
    unity = if op == :suma, do: {:numb, 0}, else: {:numb, 1}
    mmult(aast, {{:m, op}, [expr, unity]}, map)
  end

  def par_inspect({ast, plexp}) do
    listornot(ast) <> "<==" <> listornot(plexp)
  end

  def listornot(plexp) do
    cond do
      is_list(plexp) -> "[" <> (Enum.map(plexp, &Exun.UI.tostr(&1)) |> Enum.join(";"))
      true -> Exun.UI.tostr(plexp)
    end
  end

  @doc """
  Get n sets (size of abstract list) from a list of m size (expression)
  this produces a quasy cartesian product for all combinations between
  right and left lists. For example, if left and right has the same
  size n, we will try all possible combinations one to one. If ther is more
  elements on right list then we also must handle partitions (tuples) and
  order inside... a mess you know
  ```
      iex(1)> Exun.Pattern.combin_expand {{:m,:mult},[1,2,3]},{{:m,:mult},["a","b","c"]}
    [
      ["a", "b", "c"],
      ["a", "c", "b"],
      ["b", "a", "c"],
      ["b", "c", "a"],
      ["c", "a", "b"],
      ["c", "b", "a"]
    ]
      iex(2)> Exun.Pattern.combin_expand {{:m,:mult},[1,2]},{{:m,:mult},["a","b","c"]}
    [
      ["a", {"b", "c"}],
      [{"b", "c"}, "a"],
      ["a", {"c", "b"}],
      [{"c", "b"}, "a"],
      ["b", {"a", "c"}],
      [{"a", "c"}, "b"],
      ["b", {"c", "a"}],
      [{"c", "a"}, "b"],
      ["c", {"a", "b"}],
      [{"a", "b"}, "c"],
      ["c", {"b", "a"}],
      [{"b", "a"}, "c"]
    ]
  ```
  """
  def combin_expand(aast, east) do
    # Get n sets (size of abstract list) from a list of m size (expression)
    combin(aast, east)
    # |> IO.inspect(label: "Combined")
    # Combine order for each set, mult and sum are commutative
    # Elevate list of a single element to the element [e] -> e
    |> Enum.map(fn sl ->
      Enum.map(sl, fn el ->
        if is_list(el) and length(el) == 1, do: List.first(el), else: el
      end)
    end)
    # |> IO.inspect(label: "Elevate list of single element")
    |> Enum.reduce([], fn el, ac ->
      expand_order(el) ++ ac
    end)
  end

  @doc """
  deriv match in two situations: if name is yet in map, its definition must be equal
  to exp. If not it will match if we can integrate expr; initially we ca use symbolic
  integration, but if in conditions name is used without deriv, the condition will no
  be true if we can not integrate it.
  """
  def mderiv(dr = {:deriv, ef = {:vari, _func}, {:vari, var}}, expr, map) do
    case Map.fetch(map, ef) do
      {:ok, funcvalue} ->
        derivate = coll({:deriv, funcvalue, {:vari, var}})
        if eq(derivate, expr), do: [{:ok, map}], else: [{:ko, map}]

      :error ->
        [checkmap(map, dr, expr)]
        # {res, newmap} = checkmap(map, dr, expr)
        # Include integral, or not include, that is the question

        # if res == :ok do
        #  integral = {:integ, expr, {:vari, var}}
        #  [checkmap(newmap, ef, integral)]
        # else
        #  [{:ko, map}]
        # end
    end
  end

  @doc """
  Match integral, only allowed as abstract form "$f,x"
  """
  def minteg(itr = {:integ, ef = {:vari, _func}, {:vari, var}}, expr, map) do
    case Map.fetch(map, ef) do
      {:ok, funcvalue} ->
        deriv_expr = coll({:deriv, expr, {:vari, var}})
        if eq(deriv_expr, funcvalue), do: [{:ok, map}], else: [{:ko, map}]

      :error ->
        [checkmap(map, itr, expr)]
        # {res, newmap} = checkmap(map, itr, expr)

        # if res == :ok do
        #  deriv = coll({:deriv, expr, {:vari, var}})
        #  [checkmap(newmap, {:vari, func}, deriv)]
        # else
        #  [{:ko, map}]
        # end
    end
  end

  @doc """
  Match a function definition as matching f(x,y) <-> "x*y",
  so put in map the match f=x*y if f is not yet defined or it is
  and equals x*y  If we use a pattern like f(x) then x must be in the expression expr
  in any way. If we use f(g(x)) then g(x) must be in the expression also
  """
  def mfdef(acall1 = {:fcall, _, a1}, acall2 = {:fcall, _, a2}, mainmap) do
    if(length(a1) != length(a2)) do
      [{:ko, mainmap}]
    else
      # Try match arguments in same order
      mlist(a1, a2, mainmap)
      # Set fname in all matching maps
      |> Enum.map(fn {_, smap} ->
        checkmap(smap, acall1, acall2)
      end)
    end
  end

  def mfdef({:fcall, name, args}, expr, map) do
    vie = vars_of_expr(expr)
    # Suppose args holds list of simple variables that *must* be used in expression
    if Enum.all?(args, &(&1 in vie)) do
      # args of pattern will be matched this way applying sum and product of
      # them and try to match. Try to identify f is a huge problem. For example
      # f(g(x),y) :: x^y+x you cannot identify g, but in
      # f(g(x),y) :: x^2+y then g(x)=x^2 so f(g(x),y)=g(x)+y
      # May be trying to expand_order from + and * over all arguments of f
      # and try match each set with the expression would help
      # Try with sum and product for now:
      sum_try = mnode({{:m, :suma}, args}, expr, map)
      mul_try = mnode({{:m, :mult}, args}, expr, map)
      # And then check aginst definition of fcall, name
      (sum_try ++ mul_try)
      |> Enum.reject(fn {res, _map} -> res != :ok end)
      |> Enum.map(fn {_res, map} ->
        checkmap(map, {:fcall, name, args}, expr)
      end)
    else
      [{:ko, map}]
    end
  end

  def mlist(a1, a2, mainmap) when is_list(a1) and is_list(a2) do
    List.zip([a1, a2])
    # |> IO.inspect(label: "mlist zipped")
    |> Enum.reduce([{:ok, mainmap}], fn {ast, exp}, maps ->
      Enum.reduce(maps, [], fn {res, smap}, acu ->
        if res == :ok do
          mnode(ast, exp, smap) ++ acu
        else
          [{:ko, smap} | acu]
        end
      end)
    end)

    # |> Enum.reject(fn {res, _} -> res != :ok end)
  end

  def checkmap(map, key, val) do
    case Map.fetch(map, key) do
      {:ok, mapval} ->
        if Exun.Eq.eq(val, mapval) do
          {:ok, map}
        else
          {:ko, map}
        end

      _ ->
        {:ok, Map.put(map, key, val)}
    end
  end

  @doc """
  TODO: External testing, function roottype returns an atom with the type of the
  root tuple of expression. If you want to check if an expression is a symbolic
  integral, roottype(expr) must not be :integ
  For now, it returns true if there is not :integ inside ast
  """
  def check_conds(map, cnd) do
    if cnd == [],
      do: true,
      else:
        cnd
        # |> IO.inspect(label: "conditions")
        |> Enum.map(fn exp ->
          Exun.Integral.symbinteg(
            Exun.ast_eval(exp, map)
            |> IO.inspect(label: "evaluted cond")
          )

          # |> IO.inspect(label: "In cond")
        end)
        |> Enum.all?(& &1)
  end

  @doc """
  Find vars of an expression
  """
  def vars_of_expr(ast) when is_tuple(ast) do
    vars_of_expr(MapSet.new(), ast)
  end

  def vars_of_expr(mapset, ast) when is_tuple(ast) do
    case ast do
      {:vari, _} ->
        MapSet.put(mapset, ast)

      {{:m, _}, lst} ->
        vars_of_expr(mapset, lst)

      {:fcall, _name, args} ->
        vars_of_expr(mapset, args)

      {:numb, _} ->
        mapset

      {:unit, _, _} ->
        mapset

      {_, a, b} ->
        mapset
        |> vars_of_expr(a)
        |> vars_of_expr(b)

      {_, a} ->
        mapset
        |> vars_of_expr(a)
    end
  end

  def vars_of_expr(mapset, list) when is_list(list) do
    list
    |> Enum.reduce(mapset, fn el, ac ->
      vars_of_expr(ac, el)
    end)
  end

  @doc """
  Extract sublist of l of size sizes
  extract [1,2,3,4,5],[1,2,2] -> [[1],[2,3],[4,5]]
  """
  def extract(l, sizes) do
    {_, res} =
      Enum.reduce(sizes, {0, []}, fn el, {ndx, res} ->
        {ndx + el, [Enum.slice(l, ndx, el)] ++ res}
      end)

    res |> Enum.reverse()
  end

  defp sfi(number, 1, _), do: [[number]]
  defp sfi(number, 2, _), do: for(k <- 1..floor(number / 2), do: [k, number - k])

  defp sfi(number, ngs, pivot) do
    seed = sfi(number - pivot, ngs - 1, 1)
    pivots = List.duplicate(pivot, length(seed))

    ((List.zip([pivots, seed])
      |> Enum.map(fn e -> Tuple.to_list(e) |> List.flatten() end)) ++
       if pivot < floor(number / ngs) do
         sfi(number, ngs, pivot + 1)
       else
         []
       end)
    |> Enum.reject(fn set ->
      {_, res} =
        Enum.reduce(set, {1, false}, fn el, {cv, res} ->
          {el, res or el < cv}
        end)

      res
    end)
  end

  @doc """
  Sets each of size group than sums number
  Exun.Pattern.sizesets 7,4 ->
    [1, 1, 4, 1], [1, 1, 3, 2], [1, 2, 3, 1], [1, 2, 2, 2]]
  If you have a {{:m,op}, lst} and wants to pattern match lst against an abstract sum
  with 'n' variables, for example match '1+2*x+3*x^2+6*x^3' with an abstract tree like
  'a^2+b+c^3' so a=sqrt(3)*x, b=1+2*x, c=crt(6)*x we have to try all possible groupings
  of the sum expression
  """
  def sizesets(number, group), do: sfi(number, group, 1)

  @doc """
  Make any possible sets of size n from list without order
  takeany([],[1,2,3,4],3) ->
    [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
  """
  def takeany(_, [], _), do: []

  def takeany(prefix, l, 1) do
    for k <- l, do: prefix ++ [k]
  end

  def takeany(prefix, [h | t], n) do
    takeany(prefix ++ [h], t, n - 1) ++
      takeany(prefix, t, n)
  end

  @doc """
  Get any combination from list that supports ssets spec
  """
  def combin({{:m, op}, l1}, {{:m, op}, l2}) do
    sizesets(length(l2), length(l1))
    |> Enum.reduce([], fn el, ac ->
      ac ++ rcombin(el, l2, [])
    end)
    |> Enum.reverse()
  end

  def rcombin(ssets = [hs | ts], list, presolution) when is_number(hs) do
    cond do
      allones?(ssets) ->
        [list]

      ts == [] ->
        presolution ++ [list]

      hs == 1 ->
        # Number of 1's
        unos = Enum.count(ssets, &(&1 == 1))
        disperse = takeany([], list, unos)
        remain = Enum.map(disperse, &(list -- &1))

        toset =
          Enum.map(disperse, fn sol ->
            Enum.map(sol, &[&1])
          end)

        reduced_ssets = Enum.slice(ssets, unos, length(ssets))

        result =
          List.zip([remain, toset])
          |> Enum.map(fn {rem, fe} ->
            rcombin(reduced_ssets, rem, fe)
          end)

        [_hhs | tts] = reduced_ssets
        if tts != [], do: concatlist(result), else: result

      hs > 1 ->
        disperse = takeany([], list, hs)
        remain = Enum.map(disperse, &(list -- &1))
        toset = Enum.map(disperse, &(presolution ++ [&1]))

        result =
          List.zip([remain, toset])
          |> Enum.map(fn {rem, fe} ->
            rcombin(ts, rem, fe)
          end)

        [_hhs | tts] = ts
        if tts != [], do: concatlist(result), else: result
    end
  end

  def concatlist(lol) do
    Enum.reduce(lol, [], fn el, ac ->
      el ++ ac
    end)
  end

  def allones?(list), do: Enum.reduce(list, true, &(&2 and &1 == 1))

  @doc """
  take a list an generate all possible list varying order of elements
  list can be of tye [a, [b,c],d] but b and c cannot be lists
  must produce
  [
    [a,[b,c],d], [a,[c,b],d],
    [a,d,[b,c]], [a,d,[c,b]],
    [d,a,[b,c]], [d,a,[c,b]],
    [[b,c],a,d], [[b,c],d,a],
    [[c,b],a,d], [[c,b],d,a],
  ]
  """
  def expand_order([a]), do: [a]

  def expand_order(list) when is_list(list) do
    list
    |> Enum.map(&single_expand_order(&1))
    |> permute()
  end

  def permute(list) do
    subpermute(list)
    |> Enum.map(fn el ->
      Enum.map(el, fn item -> if is_list(item), do: List.to_tuple(item), else: item end)
      |> single_expand_order()
    end)
    |> List.flatten()
    |> Enum.map(fn el ->
      # Very, very ugly code...
      if is_tuple(el) and is_tuple(elem(el, 0)) and :m != elem(elem(el, 0), 0) do
        Tuple.to_list(el)
      else
        el
      end
    end)
    |> Enum.chunk_every(length(list))
  end

  @doc """
  Expand sublists taking one lement from each and building a new list
  [[a], [b,c]] -> [[a,b],[a,c]]
  [[a,b]] -> [a,b]
  """
  def subpermute([a]), do: a

  def subpermute(list) when is_list(list) do
    sizes = Enum.map(list, &length(&1))
    cycles = Enum.reduce(sizes, 1, &(&1 * &2))

    for n <- 0..(cycles - 1) do
      mklist_byindex(list, mknumber(n, sizes))
    end
  end

  def mknumber(n, [_a]), do: [n]

  def mknumber(n, [h | t]) do
    [rem(n, h)] ++ mknumber(floor(n / h), t)
  end

  def mklist_byindex(list, indexes) do
    Enum.zip([list, indexes])
    |> Enum.map(fn {sublist, index} ->
      Enum.at(sublist, index)
    end)
  end

  def single_expand_order(list) when is_list(list) do
    case list do
      [a, b] ->
        [[a, b], [b, a]]

      _ ->
        for item <- list do
          remain = List.delete(list, item)
          seo_remain = single_expand_order(remain)
          Enum.map(seo_remain, &([item] ++ &1))
        end
        |> List.flatten()
        |> Enum.chunk_every(length(list))
    end
  end

  def single_expand_order(a), do: [a]

  @doc """
  Split expression in two ast, one of them without vari x
  These two ast only can be {:m,sum} or {:m,:mult} for now
  """
  def split({{:m, op}, lst}, var = {:vari, _c}) do
    {wv, wov} =
      Enum.map(lst, fn subexp ->
        {subexp, var in vars_of_expr(subexp)}
      end)
      |> Enum.reduce({[], []}, fn {subexp, res}, {withvar, withoutvar} ->
        case res do
          true -> {[subexp | withvar], withoutvar}
          false -> {withvar, [subexp | withoutvar]}
        end
      end)

    [
      {{:m, op}, wv},
      {{:m, op}, wov}
    ]
  end
end
