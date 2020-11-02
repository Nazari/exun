defmodule Exun.Simpl do
  alias Exun.Math
  alias Exun.Collect
  alias Exun.Unit
  alias Exun.Eq

  @muno {:numb, -1.0, 1.0}
  @zero {:numb, 0.0, 1.0}
  @uno {:numb, 1.0, 1.0}
  @moduledoc """
  Simplify expressions
  """
  @doc """
  Recursively try to simplify expression. Multiple tries are performed.
  For a more agressive simplify, use Exun.Collect.coll
  """
  def mkrec(tree) do
    ntree = mk(tree)

    if Eq.eq(ntree, tree),
      do: ntree,
      else: mkrec(ntree)
  end

  # simplify
  defp mk({:minus, {:minus, a}}), do: mk(a)

  defp mk({:minus, {{:m, :mult}, list}}) do
    {res, nlist} = collect_minus(list)

    if res do
      mk({{:m, :mult}, nlist})
    else
      {:minus, mk({{:m, :mult}, list})}
    end
  end

  defp mk({:minus, @zero}), do: @zero
  defp mk({:minus, a}), do: {:minus, mk(a)}

  defp mk({:unit, val, @uno}), do: mk(val)
  defp mk({:unit, val, ut}), do: Unit.toSI({:unit, mk(val), mk(ut)})

  defp mk({:elev, _, @zero}), do: @uno
  defp mk({:elev, a, @uno}), do: mk(a)
  defp mk({:elev, @uno, _}), do: @uno
  defp mk({:elev, {:numb, base, d1}, {:numb, -1, 1}}), do: {:numb, d1, base}

  defp mk({:elev, {:numb, base, d1}, {:numb, exp, d2}}),
    do: {:numb, :math.pow(base,exp/d2), :math.pow(d1,exp/d2)}

  defp mk({:elev, {:elev, base, e1}, e2}), do: {:elev, mk(base), mk(Math.mult(e1, e2))}

  defp mk({:elev, {:unit, uv, ut}, expon}),
    do: {:unit, mk({:elev, uv, expon}), mk({:elev, ut, expon})}

  defp mk({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    # Simplify each component of the list
    lst =
      lst
      # |> IO.inspect(label: "pre mk #{op}")
      |> Enum.map(&mkrec(&1))

    # |> IO.inspect(label: ":m foreach el mkrec done")

    # |> IO.inspect(label: "post mk #{op} mk each")

    # Promote sublist, so if ther is a element in lst of class {:m,op}
    # include sublist in main list
    lst =
      Enum.reduce(lst, [], fn el, ac ->
        case el do
          {{:m, ^op}, sublist} -> sublist ++ ac
          other -> [other | ac]
        end
      end)
      |> Enum.sort(&Eq.smm(&1, &2))

    unity = if op == :suma, do: @zero, else: @uno
    ufc = if op == :suma, do: &Math.suma/2, else: &Math.mult/2

    # Collect numbers and units and simplify
    lst = collect_literals(op, lst, ufc, unity)

    # Remove zeroes or ones, 0+any=any, 1*any=any and may be the nil
    # introduced by the last command
    lst = Enum.reject(lst, &(&1 == unity or &1 == nil))
    # |> IO.inspect(label: "post literals")

    # if a multiple mult {:m,:mult} check if zero is a component
    if op == :mult and @zero in lst do
      @zero
    else
      case length(lst) do
        # No more elements in list, return unity
        0 ->
          unity

        # Only one element, replace {{}:m,op},lst} with it
        1 ->
          List.first(lst)

        # Let's play
        _ ->
          # IO.inspect(lst,label: "lst for pivot")
          {pivot, base, counts} = get_base(op, lst)
          # |> IO.inspect(label: "pivot,base,counts")

          case counts do
            1 ->
              # No match on more than one, leave as is
              {{:m, op}, lst}

            _ ->
              isol = get_isol(base, lst)
              coefs = get_coefs(isol)
              rest = get_rest(isol)

              isolp =
                mkrec(
                  case op do
                    :suma ->
                      {{:m, :mult}, [pivot, {{:m, :suma}, coefs}] |> Enum.sort(&Eq.smm(&1, &2))}

                    :mult ->
                      {:elev, pivot, {{:m, :suma}, coefs |> Enum.sort(&Eq.smm(&1, &2))}}
                  end
                )

              case length(rest) do
                0 -> isolp
                _ -> {{:m, op}, [isolp | rest] |> Enum.sort(&Eq.smm(&1, &2))}
              end
          end
      end
    end
  end

  defp mk({:fcall, name, lst}) when is_list(lst) do
    args = Enum.map(lst, &Collect.coll/1)
    Exun.Fun.fcall(name, args)
  end

  defp mk({:deriv, a, {:vari, x}}), do: Exun.Der.deriv(mk(a), x)
  defp mk({:integ, f, v = {:vari, _}}), do: Exun.Integral.integ(mk(f), v)
  defp mk({op, a, b}), do: {op, mk(a), mk(b)}

  # Fallthrough
  defp mk(tree) do
    tree
  end

  defp get_isol(base, lst) do
    List.zip([lst, base])
    |> Enum.reduce([], fn {a, res}, ac ->
      case res do
        {:ok, b} ->
          [{a, b} | ac]

        {:err, _} ->
          [{a, nil} | ac]
      end
    end)
    |> Enum.reverse()
  end

  defp get_coefs(isol) do
    isol
    |> Enum.filter(fn {_, b} -> b != nil end)
    |> Enum.reduce([], fn {_, b}, ac ->
      [b | ac]
    end)
    |> Enum.reverse()
  end

  defp get_rest(isol) do
    isol
    |> Enum.filter(fn {_, b} -> b == nil end)
    |> Enum.reduce([], fn {a, _}, ac ->
      [a | ac]
    end)
    |> Enum.reverse()
  end

  defp get_base(op, lst) do
    pivots =
      for pivot <- lst do
        {pivot,
         Enum.reduce(lst, [], fn expr, ac ->
           [cbs(op, pivot, expr) | ac]
         end)
         |> Enum.reverse()}
      end

    counts =
      pivots
      |> Enum.reduce([], fn {pivot, bases}, ac ->
        [
          {pivot, bases,
           bases
           |> Enum.reduce(0, fn {result, _}, ac ->
             case result do
               :ok -> ac + 1
               _ -> ac
             end
           end)}
          | ac
        ]
      end)
      |> Enum.reverse()

    maxbase(counts)
  end

  defp cbs(op, a, a) when op in [:suma, :mult] do
    {:ok, @uno}
  end

  defp cbs(op, {:minus, a}, a) when op in [:suma, :mult] do
    {:ok, @muno}
  end

  defp cbs(op, {:elev, a, e1}, {:elev, a, e2}) do
    case op do
      :suma -> {:err, nil}
      :mult -> {:ok, mk(Math.divi(e2, e1))}
    end
  end

  defp cbs(op, a, {:elev, a, b}) do
    case op do
      :suma -> {:ok, mk({:elev, a, mk(Math.rest(b, @uno))})}
      :mult -> {:ok, b}
    end
  end

  defp cbs(:suma, a, {{:m, :mult}, lst}) do
    cond do
      a in lst ->
        {:ok, {{:m, :mult}, lst |> List.delete(a)}}

      true ->
        {:err, nil}
    end
  end

  defp cbs(_op, _t1, _t2) do
    {:err, nil}
  end

  defp maxbase([a]), do: a
  defp maxbase([h | t]), do: Enum.reduce(t, h, &maxbasef/2)

  defp maxbasef(a1 = {_, _, c1}, a2 = {_, _, c2}) do
    if c1 > c2, do: a1, else: a2
  end

  defp collect_literals(op, lst, ufc, unity) do
    {n, u, lst} =
      Enum.reduce(lst, {unity, nil, []}, fn el, {nd_ac = {:numb, _, _}, u_ac, rest} ->
        case el do
          nd = {:numb, _, _} ->
            {ufc.(nd_ac, nd), u_ac, rest}

          u = {:unit, u_nd = {:numb, _, _}, u_t} ->
            if u_ac == nil do
              {nd_ac, u, rest}
            else
              {:unit, acu_nd, acu_t} = u_ac

              case op do
                :mult ->
                  sou = {:unit, Math.mult(acu_nd, u_nd), Math.mult(acu_t, u_t)}
                  {nd_ac, sou, rest}

                :suma ->
                  sou =
                    case Exun.Unit.sum(u_ac, u) do
                      {:err, msg} -> throw(msg)
                      {:ok, unit} -> unit
                    end

                  {nd_ac, sou, rest}
              end
            end

          other ->
            {nd_ac, u_ac, [other | rest]}
        end
      end)

    case {n, u} do
      {^unity, nil} -> lst
      {^unity, unit} -> [unit | lst]
      {number, nil} -> [number | lst]
      {nd, {:unit, und, tree}} -> [{:unit, Math.mult(nd, und), tree} | lst]
    end

    # |> IO.inspect(label: "final unit,number,lst")
  end

  # return {flag,newlist} if l is from mult and can extract signs
  # from its operands
  def acollect_minus(l) do
    {false, l}
  end

  def collect_minus(l) do
    {res, nl, _} =
      Enum.reduce(l, {false, [], false}, fn opand, {flag, nl, selected} ->
        case opand do
          {:minus, minusop} ->
            nflag = if flag, do: false, else: true

            if selected do
              {nflag, [opand | nl], true}
            else
              {nflag, [minusop | nl], true}
            end

          _ ->
            {flag, [opand | nl], selected}
        end
      end)

    {res, nl |> Enum.reverse()}
    # |> IO.inspect(label: "collected minus")
  end

end
