defmodule Exun.Simpl do
  import Exun.Fun
  alias Exun.Collect
  alias Exun.Unit
  import Exun.Eq

  @zero {:numb, 0}
  @uno {:numb, 1}

  @moduledoc """
  Simplify expressions
  """
  def mkrec(tree) do
    ntree = mk(tree)

    if eq(ntree, tree),
      do: ntree,
      else: mkrec(ntree)
  end

  # simplify
  defp mk({:minus, {:minus, a}}), do: mk(a)
  defp mk({:minus, a}), do: {:minus, mk(a)}
  defp mk({:numb, n}), do: if(floor(n) == n, do: {:numb, floor(n)}, else: {:numb, n})

  defp mk({:unit, val, {:numb, 1}}), do: mk(val)
  defp mk({:unit, val, ut}), do: Unit.toSI({:unit, mk(val), mk(ut)})

  defp mk({:elev, _, @zero}), do: @uno
  defp mk({:elev, a, @uno}), do: mk(a)
  defp mk({:elev, @uno, _}), do: @uno
  defp mk({:elev, {:numb, base}, {:numb, exp}}), do: {:numb, :math.pow(base, exp)}
  defp mk({:elev, {:elev, base, e1}, e2}), do: {:elev, mk(base), mk(mult(e1, e2))}

  defp mk({:elev, {:unit, uv, ut}, expon}),
    do: {:unit, mk({:elev, uv, expon}), mk({:elev, ut, expon})}

  defp mk({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    # Simplify each component of the list
    lst =
      lst
      # |> IO.inspect(label: "pre mk #{op}")
      |> Enum.map(fn el -> mkrec(el) end)

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
      |> Enum.sort(&smm(&1,&2))

    unity = if op == :suma, do: @zero, else: @uno
    ufc = if op == :suma, do: &+/2, else: &*/2

    # Collect numbers and units and simplify
    lst = collect_literals(op, lst, ufc, unity)
    # If op is mult, collect :minus to the first element
    lst = if op == :mult, do: collect_minus(lst), else: lst
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
              {{:m, op}, lst}

            _ ->
              isol = get_isol(base, lst)
              coefs = get_coefs(isol)
              rest = get_rest(isol)

              isolp =
                case op do
                  :suma ->
                    {{:m, :mult}, [pivot, {{:m, :suma}, coefs}]|>Enum.sort(&smm(&1,&2))}

                  :mult ->
                    {:elev, pivot, {{:m, :suma}, coefs |> Enum.sort(&smm(&1,&2))}}
                end

              case length(rest) do
                0 -> isolp
                _ -> {{:m, op}, [isolp | rest] |> Enum.sort(&smm(&1,&2))}
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

  def get_isol(base, lst) do
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

  def get_coefs(isol) do
    isol
    |> Enum.filter(fn {_, b} -> b != nil end)
    |> Enum.reduce([], fn {_, b}, ac ->
      [b | ac]
    end)
    |> Enum.reverse()
  end

  def get_rest(isol) do
    isol
    |> Enum.filter(fn {_, b} -> b == nil end)
    |> Enum.reduce([], fn {a, _}, ac ->
      [a | ac]
    end)
    |> Enum.reverse()
  end

  def get_base(op, lst) do
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

  defp cbs(op, {:elev, a, e1}, {:elev, a, e2}) do
    case op do
      :suma -> {:err, nil}
      :mult -> {:ok, mk(divi(e2, e1))}
    end
  end

  defp cbs(op, a, {:elev, a, b}) do
    case op do
      :suma -> {:ok, mk({:elev, a, mk(rest(b, @uno))})}
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
      Enum.reduce(lst, {unity, nil, []}, fn el, {{:numb, n_ac}, u_ac, rest} ->
        case el do
          {:numb, n} ->
            {{:numb, ufc.(n_ac, n)}, u_ac, rest}

          u = {:unit, {:numb, u_n}, u_t} ->
            if u_ac == nil do
              {{:numb, n_ac}, u, rest}
            else
              {:unit, {:numb, acu_n}, acu_t} = u_ac

              case op do
                :mult ->
                  sou = {:unit, {:numb, acu_n * u_n}, mult(acu_t, u_t)}
                  {{:numb, n_ac}, sou, rest}

                :suma ->
                  sou =
                    case Exun.Unit.sum(u_ac, u) do
                      {:err, msg} -> throw(msg)
                      {:ok, unit} -> unit
                    end

                  {{:numb, n_ac}, sou, rest}
              end
            end

          other ->
            {{:numb, n_ac}, u_ac, [other | rest]}
        end
      end)

    case {n, u} do
      {^unity, nil} -> lst
      {^unity, unit} -> [unit | lst]
      {number, nil} -> [number | lst]
      {{:numb, n}, {:unit, {:numb, un}, tree}} -> [{:unit, {:numb, n * un}, tree} | lst]
    end

    # |> IO.inspect(label: "final unit,number,lst")
  end

  def collect_minus(l) do
    l
  end
end
