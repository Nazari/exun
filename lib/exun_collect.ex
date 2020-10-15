defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Unit

  @zero {:numb, 0}
  @uno {:numb, 1}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @invalid_unit_operation "Inconsistent unit operation"
  @doc """
  Main collecting function. Try to simplify tree without chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> mkrec()
      # |> IO.inspect(label: "make01,mkrec->norm")
      |> norm()
      # |> IO.inspect(label: "make02, norm->mkrec")
      |> mkrec()
      # |> IO.inspect(label: "make02, mkrec->solve_literals")
      |> solve_literals()
      # |> IO.inspect(label: "make03,solve_literals->mkrec")
      |> mkrec()
      # |> IO.inspect(label: "make04,mk->denorm")
      |> denorm()

    if Exun.eq(newtree, tree), do: newtree, else: coll(newtree)
  end

  defp mkrec(tree) do
    ntree = mk(tree)

    if Exun.eq(ntree, tree),
      do: ntree,
      else: mkrec(ntree)
  end

  defp solve_literals({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    lst =
      lst
      |> Enum.map(fn el ->
        case el do
          {{:m, _}, _} -> solve_literals(el)
          _ -> el
        end
      end)

    numbers = Enum.filter(lst, &(elem(&1, 0) == :numb))
    lst = lst -- numbers

    collnumb =
      case length(numbers) do
        0 ->
          nil

        1 ->
          numbers |> List.first()

        _ ->
          {:numb,
           Enum.reduce(numbers, if(op == :suma, do: 0, else: 1), fn {:numb, n}, ac ->
             case op do
               :suma -> ac + n
               :mult -> ac * n
             end
           end)}
      end

    units = Enum.filter(lst, &(elem(&1, 0) == :unit))
    lst = lst -- units

    collunit =
      case length(units) do
        0 ->
          nil

        1 ->
          units |> List.first()

        _ ->
          [hu | tu] = units

          Enum.reduce(tu, hu, fn el, ac ->
            case op do
              :suma ->
                case Unit.sum(op, ac, el, %{}) do
                  {:ok, res} -> res
                  {:err, msg} -> throw(msg)
                end

              :mult ->
                {_, valunit, treeunit} = ac
                {_, valel, treeel} = el
                {:unit, {:mult, valunit, valel}, {:mult, treeunit, treeel}}
            end
          end)
      end

    case {collnumb, collunit} do
      {nil, nil} ->
        {{:m, op}, lst}

      {_, nil} ->
        {{:m, op}, [collnumb | lst]}

      {nil, _} ->
        {{:m, op}, [Unit.toSI(collunit) | lst]}

      _ ->
        {{:m, op}, [Unit.toSI(mk({op, collnumb, collunit})) | lst]}
    end
  end

  defp solve_literals(tree) do
    tree
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

  defp mk({:mult, a, {:divi, @uno, b}}), do: {:divi, mk(a), mk(b)}
  defp mk({:mult, {:divi, @uno, b}, a}), do: {:divi, mk(a), mk(b)}

  defp mk({:mult, @muno, {:suma, a, {:mult, @muno, b}}}), do: {:rest, b, a}
  defp mk({:mult, {:suma, a, {:mult, @muno, b}}, @muno}), do: {:rest, b, a}

  defp mk({:mult, {:elev, a, e1}, {:elev, a, e2}}),
    do: {:elev, mk(a), mk({:suma, mk(e1), mk(e2)})}

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
    do: {:elev, mk(a), mk({:rest, mk(e1), @uno})}

  defp mk({:divi, a, {:elev, a, e1}}),
    do: {:divi, @uno, mk({:rest, mk(e1), @uno})}

  defp mk({:divi, {:unit, n1, a1}, {:unit, n2, a2}}),
    do: {:unit, mk({:divi, n1, n2}), mk({:divi, a1, a2})}

  defp mk({:elev, _, @zero}), do: @uno
  defp mk({:elev, a, @uno}), do: mk(a)
  defp mk({:elev, @uno, _}), do: @uno
  defp mk({:elev, {:elev, base, e1}, e2}), do: {:elev, mk(base), mk({:mult, e1, e2})}
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
    args = Enum.map(lst, &coll/1)
    Exun.Fun.fcall(name, args)
  end

  defp mk({:deriv, a, {:vari, x}}) do
    Exun.Fun.deriv(a, x)
    # |> IO.inspect(label: "Deriving")
  end

  defp mk({op, a, b}), do: {op, mk(a), mk(b)}

  # Fallthrough
  defp mk(tree) do
    tree
  end

  @doc """
  Normalize tree so :mult and :suma are converted to list:
  {:mult,{:mult,a,b},c} -> {{:m,:mult},[a,b,c]}
  """

  def norm({:divi, a, b}) do
    norm({{:m, :mult}, [norm(a), {:elev, norm(b), {:numb, -1}}] |> Enum.sort()})
  end

  def norm({:rest, a, b}) do
    norm({{:m, :suma}, [norm(a), chsign(norm(b))] |> Enum.sort()})
  end

  def norm({op, a, b}) when op in [:suma, :mult] do
    an = norm(a)
    bn = norm(b)

    case {an, bn} do
      {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
        {{:m, op}, (l1 ++ l2) |> Enum.sort()}

      {{{:m, ^op}, l1}, ^bn} ->
        {{:m, op}, [bn | l1] |> Enum.sort()}

      {^an, {{:m, ^op}, l2}} ->
        {{:m, op}, [an | l2] |> Enum.sort()}

      {^an, ^bn} ->
        {{:m, op}, [an, bn] |> Enum.sort()}
    end
  end

  def norm({op, a, b}) do
    {op, norm(a), norm(b)}
  end

  def norm(other) do
    other
  end

  @doc """
  Change sign of AST
  """
  def chsign({:vari, var}) do
    {:mult, @muno, {:vari, var}}
  end

  def chsign({:unit, a, b}) do
    {:unit, chsign(a), b}
  end

  def chsign({:elev, a, b}) do
    {:mult, @muno, {:elev, a, b}}
  end

  def chsign({:numb, n}) do
    {:numb, -n}
  end

  def chsign({:mult, a, b}) do
    {:mult, chsign(a), b}
  end

  def chsign({:divi, a, b}) do
    {:divi, chsign(a), b}
  end

  def chsign({:suma, a, b}) do
    {:suma, chsign(a), chsign(b)}
  end

  def chsign({:rest, a, b}) do
    {:rest, b, a}
  end

  def chsign({{:m, :mult}, lst}) do
    {{:m, :mult}, [@muno | lst] |> Enum.sort()}
  end

  def chsign({{:m, :suma}, lst}) do
    {{:m, :suma},
     Enum.map(lst, fn el ->
       chsign(el)
     end)
     |> Enum.sort()}
  end

  @doc """
  Change power sign of AST (expon * -1 or 1/tree)
  """
  def chpow({:vari, var}) do
    {:elev, {:vari, var}, {:numb, -1}}
  end

  def chpow({:unit, a, b}) do
    {:unit, chpow(a), chpow(b)}
  end

  def chpow({:elev, a, b}) do
    {:elev, a, chsign(b)}
  end

  def chpow({:numb, n}) do
    {:numb, 1 / n}
  end

  def chpow({:mult, a, b}) do
    {:mult, chpow(a), chpow(b)}
  end

  def chpow({:divi, a, b}) do
    {:divi, b, a}
  end

  def chpow({:suma, a, b}) do
    {:divi, @uno, {:suma, a, b}}
  end

  def chpow({:rest, a, b}) do
    {:divi, @uno, {:rest, a, b}}
  end

  def chpow({{:m, :mult}, lst}) do
    {{:m, :mult}, Enum.map(lst, fn el -> chpow(el) end)}
  end

  def chpow(suma = {{:m, :suma}, _lst}) do
    {:divi, @uno, suma}
  end

  @doc """
  Denormalize tree, reverse of norm
  """
  def denorm({{:m, op}, lista}) when op in [:suma, :mult] and is_list(lista) do
    lista = Enum.map(lista, &denorm/1)

    case length(lista) do
      0 ->
        if op == :mult, do: @uno, else: @zero

      1 ->
        List.first(lista)

      _ ->
        balance(op,lista)
        # |> IO.inspect(label: "Denormed :m")
    end
  end

  def denorm({op, l, r}) do
    # IO.inspect([op, l, r], label: "Denorming...")
    {op, denorm(l), denorm(r)}
    # |> IO.inspect(label: "Denormed #{op}")
  end

  def denorm(tree) do
    tree
    # |> IO.inspect(label: "Not possible denorm")
  end

  defp balance(op, lst) do
    cond do
      length(lst) == 1 ->
        lst |> List.first()

      true ->
        balance(
          op,
          Enum.chunk_every(lst, 2, 2, [:right])
          |> Enum.map(fn [l, r] ->
            if r != :right do
              {op, l, r}
            else
              l
            end
          end)
        )
    end
  end
end
