defmodule Exun.Simpl do
  import Exun.MProc
  import Exun.Math
  alias Exun.Collect
  alias Exun.Unit
  alias Exun.Eq

  @zero {:numb, 0}
  @uno {:numb, 1}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @invalid_unit_operation "Inconsistent unit operation"
  @moduledoc """
  Simplify expressions
  """
  def mkrec(tree) do
    ntree = mk(tree)

    if Eq.eq(ntree, tree),
      do: ntree,
      else: mkrec(ntree)
  end

  # simplify
  defp mk({:numb, n}), do: if(floor(n) == n, do: {:numb, floor(n)}, else: {:numb, n})

  defp mk({:unit, val, {:numb, 1}}), do: mk(val)
  defp mk({:unit, val, ut}), do: Unit.toSI({:unit, mk(val), mk(ut)})

  defp mk({:suma, a, @zero}), do: mk(a)
  defp mk({:suma, @zero, a}), do: mk(a)
  defp mk({:suma, {:numb, n1}, {:numb, n2}}), do: {:numb, n1 + n2}
  defp mk({:suma, {:numb, _}, {:unit, _, _}}), do: throw(@invalid_unit_operation)
  defp mk({:suma, {:unit, _, _}, {:numb, _}}), do: throw(@invalid_unit_operation)

  defp mk({:suma, u1 = {:unit, _, _}, u2 = {:unit, _, _}}) do
    case Unit.sum(:suma, u1, u2, %{}) do
      {:ok, res} -> res
      {:err, msg} -> throw(msg)
    end
  end

  defp mk({:rest, {:numb, n1}, {:numb, n2}}), do: {:numb, n1 - n2}
  defp mk({:rest, {:numb, _}, {:unit, _, _}}), do: throw(@invalid_unit_operation)
  defp mk({:rest, {:unit, _, _}, {:numb, _}}), do: throw(@invalid_unit_operation)

  defp mk({:rest, u1 = {:unit, _, _}, u2 = {:unit, _, _}}) do
    case Unit.sum(:rest, u1, u2, %{}) do
      {:ok, res} -> res
      {:err, msg} -> throw(msg)
    end
  end

  defp mk({:mult, _, @zero}), do: @zero
  defp mk({:mult, @zero, _}), do: @zero

  defp mk({:mult, @uno, a}), do: mk(a)
  defp mk({:mult, a, @uno}), do: mk(a)

  defp mk({:mult, a, a}), do: {:elev, mk(a), @dos}

  defp mk({:mult, a, {:divi, @uno, b}}), do: parallel({:divi, mk(a), mk(b)})
  defp mk({:mult, {:divi, @uno, b}, a}), do: parallel({:divi, mk(a), mk(b)})

  defp mk({:mult, @muno, {:suma, a, {:mult, @muno, b}}}), do: {:rest, b, a}
  defp mk({:mult, {:suma, a, {:mult, @muno, b}}, @muno}), do: {:rest, b, a}

  defp mk({:mult, {:elev, a, e1}, {:elev, a, e2}}),
    do: parallel({:elev, mk(a), mk({:suma, mk(e1), mk(e2)})})

  defp mk({:mult, {:numb, n1}, {:numb, n2}}), do: {:numb, n1 * n2}
  defp mk({:mult, {:unit, n2, a}, n = {:numb, _n1}}), do: {:unit, mk({:mult, n2, n}), mk(a)}
  defp mk({:mult, {:numb, n1}, {:unit, n2, a}}), do: {:unit, mk({:mult, {:numb, n1}, n2}), mk(a)}

  defp mk({:mult, {:unit, n1, a1}, {:unit, n2, a2}}),
    do: {:unit, mk({:mult, n1, n2}), mk({:mult, a1, a2})}

  defp mk({:divi, _, @zero}), do: throw("Division by 0")
  defp mk({:divi, @zero, _}), do: @zero
  defp mk({:divi, a, @uno}), do: mk(a)
  defp mk({:divi, a, a}) when a != @zero, do: @uno
  defp mk({:divi, {:numb, n1}, {:numb, n2}}), do: {:numb, n1 / n2}
  defp mk({:divi, n = {:numb, _}, {:unit, nu, a}}), do: {:unit, mk({:divi, n, nu}), mk(chpow(a))}
  defp mk({:divi, {:unit, nu, a}, n = {:numb, _}}), do: {:unit, mk({:divi, nu, n}), mk(a)}

  defp mk({:divi, {:elev, a, e1}, {:elev, a, e2}}),
    do: {:elev, mk(a), mk({:rest, mk(e1), mk(e2)})}

  defp mk({:divi, {:elev, a, e1}, a}),
    do: parallel({:elev, mk(a), mk({:rest, mk(e1), @uno})})

  defp mk({:divi, a, {:elev, a, e1}}),
    do: {:divi, @uno, mk({:rest, mk(e1), @uno})}

  defp mk({:divi, {:unit, n1, a1}, {:unit, n2, a2}}),
    do: {:unit, mk({:divi, n1, n2}), mk({:divi, a1, a2})}

  defp mk({:elev, _, @zero}), do: @uno
  defp mk({:elev, a, @uno}), do: mk(a)
  defp mk({:elev, @uno, _}), do: @uno
  defp mk({:elev, {:elev, base, e1}, e2}), do: parallel({:elev, mk(base), mk({:mult, e1, e2})})
  defp mk({:elev, {:numb, base}, {:numb, exp}}), do: {:numb, :math.pow(base, exp)}

  defp mk({:elev, {:unit, uv, ut}, expon}),
    do: {:unit, mk({:elev, uv, expon}), mk({:elev, ut, expon})}

  defp mk({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    # Remove zeroes or ones, 0+any=any, 1*any=any
    unity = if op == :suma, do: @zero, else: @uno

    lst =
      Enum.reject(lst, &(&1 == unity))
      |> Enum.map(&mk/1)

    # if a multiple mult {:m,:mult} check if zero is a component
    cond do
      op == :mult and @zero in lst ->
        @zero

      true ->
        case length(lst) do
          0 ->
            unity

          1 ->
            List.first(lst)

          _ ->
            {pivot, base, counts} = get_base(op, lst)

            case counts do
              1 ->
                {{:m, op}, lst}

              _ ->
                isol = get_isol(base, lst)
                coefs = get_coefs(isol)
                rest = get_rest(isol)

                isolp =
                  case op do
                    :suma ->
                      {:mult, pivot, {{:m, :suma}, coefs}}

                    :mult ->
                      {:elev, pivot, {{:m, :suma}, coefs}}
                  end

                case length(rest) do
                  0 -> isolp
                  _ -> {{:m, op}, [isolp | rest] |> Enum.sort()}
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
         lst
         |> Enum.reduce([], fn expr, ac ->
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

  defp cbs(op, {:elev, a, e1}, {:elev, a, e2}) do
    case op do
      :suma -> {:err, nil}
      :mult -> {:ok, mk({:divi, e2, e1})}
    end
  end

  defp cbs(op, a, {:elev, a, b}) do
    case op do
      :suma -> {:ok, mk({:elev, a, mk({:rest, b, @uno})})}
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
end
