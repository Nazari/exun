defmodule Exun.Pattern do
  @moduledoc """
  Match ASTs
  """

  def umatch(taast, texpr, tconditions \\ []) do
    match(taast, texpr, %{}, tconditions)
    |> Enum.each(fn {res, map} ->
      IO.puts("Match group #{res}")

      map
      |> Enum.each(fn {name, value} ->
        IO.puts("  #{name}\t=> " <> Exun.UI.tostr(value))
      end)
    end)
  end

  def match(taast, texpr, context, tconditions \\ []) do
    {naast, _} = Exun.parse(taast)
    {nexpr, _} = Exun.parse(texpr, context)

    aast = Exun.Eq.norm(naast)
    expr = Exun.Eq.norm(nexpr)

    conditions =
      Enum.map(tconditions, fn cnd ->
        {res, _} = Exun.parse(cnd)
        res
      end)

    match_ast(aast, expr, conditions)
  end

  def match_ast(aast, expr, conditions \\ []) do
    case mnode(aast, expr, %{}) do
      [] ->
        [{:nomatch, %{}}]

      lossol ->
        Enum.map(lossol, fn {res, map} ->
          case res do
            :ok ->
              if check_conds(map, conditions) do
                {:ok, map}
              else
                {:nocond, map}
              end

            :ko ->
              {:ko, map}
          end
        end)
    end
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
    case {aast, expr} do
      # Two numbers must match exactly
      {{:numb, n}, {:numb, n}} ->
        [{:ok, map}]

      # Base and expon must match
      {{:elev, a, b}, {:elev, c, d}} ->
        mpair({a, c}, {b, d}, map)

      # Function def from abstract ast match if vars are used in expr
      {a = {:fcall, _name, _args}, expr} ->
        mfdef(a, expr, map)

      # see mderiv
      {der = {:deriv, {:vari, _}, {:vari, _}}, expr} ->
        mderiv(der, expr, map)

      # see minteg
      {{:integ, {:vari, name}, _var}, expr} ->
        checkmap(map, name, expr)

      # Multiple sum or product. This can produce multiple tries for matching
      # We *must* check all of them,
      {a = {{:m, op}, _l1}, b = {{:m, op}, _l2}} ->
        mmult(a, b, map)

      # General match
      {{:vari, a}, expr} ->
        checkmap(map, a, expr)

      _ ->
        [{:ko, map}]
    end
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

  def mmult(aast = {{:m, op}, lsta}, east = {{:m, op}, _lste}, map) do
    # Get n sets (size of abstract list) from a list of m size (expression)
    combin(aast, east)
    # |> IO.inspect(label: "Combined")
    # Combine order for each set, mult and sum are commutative
    |> Enum.reduce([], fn el, ac ->
      expand_order(el) ++ ac
    end)
    |> IO.inspect(label: "Order Expanded")
    # zip abstract and expresion ans try to match with mnode
    |> Enum.reduce([], fn set, acc ->
      (List.zip([lsta, set])
       # |> IO.inspect(label: "Zipped")
       |> Enum.reduce([{:ok, map}], fn {abs, exp}, [{res, map}] ->
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

         # |> IO.inspect(label: "cond do")
       end)) ++ acc
    end)
    |> Enum.reject(fn {res, _} -> res == :ko end)
  end

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
  def expand_order(list) when is_list(list) do
    list
    |> Enum.map(&single_expand_order(&1))
    |> permute()
  end

  def permute(list) do
    subpermute(list)
    |> Enum.map(fn el ->
      el
      |> Enum.map(fn item -> if is_list(item), do: List.to_tuple(item), else: item end)
      |> single_expand_order()
    end)
    |> List.flatten()
    |> Enum.chunk_every(length(list))
    |> Enum.map(fn sublist ->
      Enum.map(sublist, fn item ->
        if is_tuple(item), do: Tuple.to_list(item), else: item
      end)
    end)
  end

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
  deriv match in two situations: if name is yet in map, its definition must be equal
  to exp. If not it will match if we can integrate expr; initially we ca use symbolic
  integration, but if in conditions name is used without deriv, the condition will no
  be true if we can not integrate it.
  """
  def mderiv({:deriv, {:vari, func}, {:vari, var}}, expr, map) do
    case checkmap(map, func <> "'" <> var, expr) do
      [{:ok, map}] ->
        integral = Exun.Collect.coll({:integ, expr, {:vari, var}})
        checkmap(map, func, integral)

      a ->
        a
    end
  end

  @doc """
  Match a function definition as matching f(x,y) <-> "x*y",
  so put in map the match f=x*y if f is not yet defined or it is
  and equals x*y
  """
  def mfdef({:fcall, name, args}, expr, map) do
    vie = vars_of_expr(expr)

    # Suppose args holds list of simple variables that *must* be uses in expression
    res =
      Enum.reduce(args, true, fn var, res ->
        res and var in vie
      end)

    if res, do: checkmap(map, name, expr), else: [{:ko, map}]
  end

  # May be the order is important
  def mpair({a1, e1}, {a2, e2}, map) do
    mpair_uni({a1, e1}, {a2, e2}, map) ++
      mpair_uni({a2, e2}, {a1, e1}, map)
  end

  def mpair_uni({a1, e1}, {a2, e2}, map) do
    mnode(a1, e1, map)
    |> Enum.reduce([], fn {res, rmap}, acc ->
      case res do
        :ok ->
          mnode(a2, e2, rmap)
          |> Enum.reduce(acc, fn {res2, rmap2}, acc2 ->
            [{res2, rmap2} | acc2]
          end)

        :ko ->
          acc
      end
    end)
  end

  def checkmap(map, key, val) do
    case Map.fetch(map, key) do
      {:ok, mapval} ->
        if Exun.Eq.eq(val, mapval) do
          [{:ok, map}]
        else
          [{:ko, map}]
        end

      _ ->
        [{:ok, Map.put(map, key, val)}]
    end
  end

  def check_conds(map, cnd) do
    Enum.reduce(cnd, true, fn el, ac ->
      ac and check_cond(map, el)
    end)
  end

  def check_cond(map, cnd) do
    true
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

      {:fcall, name, args} ->
        throw("Not yet")

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

  @doc """
  Rotate list keeping sizes
  rotate [[1],[2,3],[4,5]] -> [[2],[3,4],[5,1]]
  """
  def rotate(l) do
    sizes = Enum.map(l, &length(&1))
    [h | t] = l |> List.flatten()
    extract(t ++ [h], sizes)
  end

  def rotate(l = [h | t], n) do
    case n do
      0 -> l
      _ -> rotate(t ++ [h], n - 1)
    end
  end

  def sfi(number, 1, _), do: [[number]]
  def sfi(number, 2, _), do: for(k <- 1..floor(number / 2), do: [k, number - k])

  def sfi(number, ngs, pivot) do
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

  def uninner(lst, n) when n <= 0, do: lst
  def uninner(lst, n), do: uninner(concatlist(lst), n - 1)

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
end
