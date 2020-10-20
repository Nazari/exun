defmodule Exun.Pattern do
  @moduledoc """
  Match ASTs
  """

  def match(taast, texpr, context, tconditions \\ []) do
    {expr, _} = Exun.parse(texpr, context)
    {aast, _} = Exun.parse(taast)

    conditions =
      Enum.map(tconditions, fn cnd ->
        {res, _} = Exun.parse(cnd)
        res
      end)

    match_ast(aast, expr, conditions)
  end

  def match_ast(aast, expr, conditions \\ []) do
    case m(aast, expr, %{}) do
      {:ok, mapdef} ->
        check_conds(mapdef, conditions)

      _ ->
        {:nomatch}
    end
  end

  @doc """
  General matching function
  """
  def m(aast, expr, map) do
    case {aast, expr} do
      # Two numbers must match exactly
      {{:numb, n}, {:numb, n}} ->
        {:ok, map}

      # Base and expon must match
      {{:elev, a, b}, {:elev, c, d}} ->
        mpair({a, c}, {b, d}, map)

      # Function def from abstract ast match if vars are used in expr
      {{:fdef, name, args}, expr} ->
        mfdef(name, args, expr, map)

      # see mderiv
      {{:deriv, name, var}, expr} ->
        mderiv(name, var, expr, map)

      # see minteg
      {{:integ, name, _var}, expr} ->
        checkmap(map, name, expr)

      # Multiple sum or product. This can produce multiple tries for matching
      # We *must* check all of them,
      {{{:m, op}, lstaast}, {{:m, op}, lstexpr}} ->
        mm(op, lstaast, lstexpr, map)

      # General match
      {{:vari, a}, expr} ->
        checkmap(map, a, expr)

      _ ->
        {:ko, map}
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

  def mm(op, lsta, lste, map) do
    ssets = sizesets(length(lste), length(lsta))

    for ss <- ssets do
      IO.inspect(ss, label: "Sizeset")
      combin(ss, lste) |> IO.inspect(label: "Combined")
    end
  end

  @doc """
  deriv match in two situations: if name is yet in map, its definition must be equal
  to exp. If not it will match if we can integrate expr; initially we ca use symbolic
  integration, but if in conditions name is used without deriv, the condition will no
  be true if we can not integrate it.
  """
  def mderiv(name, _var, expr, map) do
    checkmap(map, name, expr)
  end

  @doc """
  Match a function definition as matching f(x,y) <-> "x*y",
  so put in map the match f=x*y if f is not yet defined or it is
  and equals x*y
  """
  def mfdef(name, args, expr, map) do
    vie = vars_of(expr)
    # Suppose args holds list of simple variables that *must* be uses in expression
    res =
      Enum.reduce(args, true, fn var, res ->
        res and var in vie
      end)

    if not res do
      {:ko, map}
    else
      case Map.fetch(map, name) do
        {:ok, val} ->
          if Exun.Eq.eq(val, expr) do
            {:ok, map}
          else
            {:ko, map}
          end

        :error ->
          {:ok, Map.put(map, name, expr)}
      end
    end
  end

  def mpair({a1, e1}, {a2, e2}, map) do
    # May be the order is important
    case m(a1, e1, map) do
      {:ok, map} ->
        m(a2, e2, map)

      {:ko, map} ->
        case m(a2, e2, map) do
          {:ok, map} ->
            m(a1, e1, map)

          _ ->
            {:ko, map}
        end
    end
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

  def check_conds(map, cnd) do
    Enum.reduce(cnd, true, fn el, ac ->
      check_cond(map, el) and ac
    end)
  end

  def check_cond(map, cnd) do
    true
  end

  @doc """
  Find vars of an expression
  """
  def vars_of(ast) do
    rvars_of(ast, MapSet.new())
  end

  def rvars_of(ast, mapset) when is_tuple(ast) do
    case ast do
      {:vari, _} ->
        MapSet.put(mapset, ast)

      {{:m, _}, lst} ->
        rvars_of(lst, mapset)

      {:fcall, name, args} ->
        rvars_of(args, MapSet.put(mapset, {:fdef, name, args}))

      {_, a, b} ->
        mapset = rvars_of(a, mapset)
        rvars_of(b, mapset)

      {_, a} ->
        rvars_of(a, mapset)
    end
  end

  def rvars_of(list, mapset) when is_list(list) do
    Enum.reduce(list, mapset, fn el, ac ->
      rvars_of(el, ac)
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
  def combin(ssets, list) do
    res = rcombin(ssets, list, [])

    {_, inners} =
      Enum.reduce(ssets, {0, nil}, fn el, {count, lastel} ->
        {if(lastel != el, do: count + 1, else: count), el}
      end)
    uninner(res, inners-2)
  end

  def uninner(lst, n) when n <= 0, do: lst
  def uninner(lst, n), do: uninner(concatlist(lst), n - 1)

  def rcombin(ssets = [hs | ts], list, presolution) do
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

        List.zip([remain, toset])
        |> Enum.map(fn {rem, fe} ->
          rcombin(reduced_ssets, rem, fe)
        end)

      true ->
        disperse = takeany([], list, hs)
        remain = Enum.map(disperse, &(list -- &1))
        toset = Enum.map(disperse, &(presolution ++ [&1]))

        List.zip([remain, toset])
        |> Enum.map(fn {rem, fe} ->
          rcombin(ts, rem, fe)
        end)
    end
  end

  def concatlist(lol) do
    Enum.reduce(lol, [], fn el, ac ->
      ac ++ el
    end)
  end

  def allones?(list), do: Enum.reduce(list, true, &(&2 and &1 == 1))
end
