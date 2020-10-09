defmodule Exun.Collect do
  alias Exun.Unit

  @zero {:numb, 0}
  @uno {:numb, 1}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @infinite {:numb, :infinite}
  @invalid_unit_operation "Inconsistent unit operation"

  def make(tree) do
    newtree = tree
    # |> IO.inspect(label: "make01,init:Antes de inspect")
    |> norm()
    # |> IO.inspect(label: "make02,norm:Antes de mkrec")
    |> mk()
    # |> IO.inspect(label: "make03,mkrec:Antes de denorm")
    |> denorm()

    if Exun.eq(newtree,tree), do: tree, else: make(newtree)
  end

  def simplify(op, lst) when op in [:suma, :mult] and is_list(lst) do
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
                {:unit, mk({:mult, valunit, valel}), mk({:mult, treeunit, treeel})}
            end
          end)
      end

    case {collnumb, collunit} do
      {nil, nil} ->
        lst

      {_, nil} ->
        [collnumb | lst]

      {nil, _} ->
        [Unit.simplify(collunit) | lst]

      _ ->
        {:unit,val,tree} = collunit
        newunit = {:unit, make({:mult,collnumb,val}), tree}
        [Unit.simplify(newunit) | lst]
      end
  end

  def get_base(op, lst) do
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

  def maxbase([a]), do: a
  def maxbase([h | t]), do: Enum.reduce(t, h, &maxbasef/2)

  def maxbasef(a1 = {_, _, c1}, a2 = {_, _, c2}) do
    if c1 > c2, do: a1, else: a2
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

  # simplify
  def mk({:suma, a, @zero}), do: make(a)
  def mk({:suma, @zero, a}), do: make(a)
  def mk({:mult, _, @zero}), do: @zero
  def mk({:mult, @zero, _}), do: @zero
  def mk({:mult, @uno, a}), do: make(a)
  def mk({:mult, a, @uno}), do: make(a)
  def mk({:mult, a, a}), do: {:elev, make(a), @dos}
  def mk({:divi, _, @zero}), do: @infinite
  def mk({:divi, @zero, _}), do: @zero
  def mk({:divi, a, @uno}), do: make(a)
  def mk({:elev, _, @zero}), do: @uno
  def mk({:elev, a, @uno}), do: make(a)
  def mk({:elev, @uno, _}), do: @uno
  def mk({:unit, val, {:numb, 1}}), do: mk(val)

  def mk(orig = {{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    lst = simplify(op, lst)

    # if a multiple mult {:m,:mult} check if zero is a component
    cond do
      op == :mult and @zero in lst ->
        @zero

      true ->
        # Remove zeroes or ones, 0+any=any, 1*any=any
        unity = if op == :suma, do: @zero, else: @uno
        lst = Enum.reject(lst, &(&1 == unity))

        case length(lst) do
          0 ->
            unity

          1 ->
            Enum.at(lst, 0)

          _ ->
            {pivot, base, counts} = get_base(op, lst)

            case counts do
              1 ->
                orig

              _ ->
                isol = get_isol(base, lst)
                coefs = get_coefs(isol)
                rest = get_rest(isol)

                case op do
                  :suma ->
                    isolp = make({:mult, pivot, mk({{:m, :suma}, coefs})})
                    {{:m, :suma}, [isolp | rest]}

                  :mult ->
                    isolp = make({:elev, pivot, mk({{:m, :suma}, coefs})})
                    {{:m, :mult}, [isolp | rest]}
                end
            end
        end
    end
  end

  # number, number
  def mk({:suma, {:numb, n1}, {:numb, n2}}) do
    {:numb, n1 + n2}
  end

  def mk({:rest, {:numb, n1}, {:numb, n2}}) do
    {:numb, n1 - n2}
  end

  def mk({:mult, {:numb, n1}, {:numb, n2}}) do
    {:numb, n1 * n2}
  end

  def mk({:divi, {:numb, n1}, {:numb, n2}}) do
    cond do
      n2 != 0 -> {:numb, n1 / n2}
      true -> {:numb, :infinite}
    end
  end

  # number, unit
  def mk({:suma, {:numb, _}, {:unit, _, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:rest, {:numb, _}, {:unit, _, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:mult, {:numb, n1}, {:unit, n2, a}}) do
    {:unit, make({:mult, {:numb, n1}, n2}), make(a)}
  end

  def mk({:divi, {:numb, n1}, {:unit, n2, a}}) do
    {:unit, make({:divi, {:numb, n1}, n2}), chpow(make(a))}
  end

  # unit, number
  def mk({:suma, {:unit, _, _}, {:numb, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:rest, {:unit, _, _}, {:numb, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:mult, {:unit, n2, a}, n = {:numb, _n1}}) do
    {:unit, make({:mult, n2, n}), make(a)}
  end

  def mk({:divi, {:unit, n1, a}, n = {:numb, _n2}}) do
    {:unit, make({:divi, n1, n}), make(a)}
  end

  # unit, unit
  def mk({:suma, u1 = {:unit, _, _}, u2 = {:unit, _, _}}) do
    case Unit.sum(:suma, u1, u2, %{}) do
      {:ok, res} -> res
      {:err, msg} -> throw(msg)
    end
  end

  def mk({:rest, u1 = {:unit, _, _}, u2 = {:unit, _, _}}) do
    case Unit.sum(:rest, u1, u2, %{}) do
      {:ok, res} -> res
      {:err, msg} -> throw(msg)
    end
  end

  def mk({:mult, {:unit, n1, a1}, {:unit, n2, a2}}) do
    {:unit, make({:mult, n1, n2}), make({:mult, a1, a2})}
  end

  def mk({:divi, {:unit, n1, a1}, {:unit, n2, a2}}) do
    {:unit, make({:divi, n1, n2}), make({:divi, a1, a2})}
  end

  def mk({op, a, b}) do
    {op, mk(a), mk(b)}
  end

  # Fallthrough
  def mk(tree) do
    tree
  end

  @doc """
  Returns commom base for two trees. Used for collect {:m,:suma}
  """
  def cbs(op, a, a) when op in [:suma, :mult] do
    {:ok, @uno}
  end

  def cbs(op, a, {:elev, a, b}) do
    case op do
      :suma -> {:ok, mk({:elev, a, mk({:rest, b, @uno})})}
      :mult -> {:ok, b}
      _ -> {:err, nil}
    end
  end

  def cbs(op, {:elev, a, b}, {:elev, a, c}) do
    case op do
      :suma -> {:err, nil}
      :mult -> {:ok, mk({:elev, a, {:rest, c, b}})}
    end
  end

  def cbs(:suma, a, {{:m, :mult}, lst}) do
    cond do
      a in lst ->
        {:ok, {{:m, :mult}, lst |> List.delete(a)}}

      true ->
        {:err, nil}
    end
  end

  def cbs(_op, _t1, _t2) do
    {:err, nil}
  end

  def scoll(tree) do
    tree
  end

  @doc """
  Normalize tree so :mult and :suma are converted to list:
  {:mult,{:mult,a,b},c} -> {{:m,:mult},[a,b,c]}
  """
  def norm({:rest, a, {{:m, :suma}, lst}}) do
    {{:m, :suma}, [norm(a) | Enum.map(lst, fn el -> chsign(norm(el)) end)]}
  end

  def norm({:rest, {{:m, :suma}, lst}, a}) do
    {{:m, :suma}, lst ++ [chsign(norm(a))]}
  end

  def norm({:divi, a, {{:m, :mult}, lst}}) do
    {{:m, :mult}, [norm(a) | Enum.map(lst, fn el -> chpow(norm(el)) end)]}
  end

  def norm({:divi, {{:m, :mult}, lst}, a}) do
    {{:m, :mult}, lst ++ [chpow(norm(a))]}
  end

  def norm({:divi, a, b}) do
    norm({:mult, a, chpow(b)})
  end

  def norm({:rest, a, b}) do
    norm({:suma, a, chsign(b)})
  end

  def norm({op, a, b}) when op in [:suma, :mult] do
    an = norm(a)
    bn = norm(b)

    case {an, bn} do
      {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
        {{:m, op}, (l1 ++ l2) |> Enum.sort()}

      {{{:m, ^op}, l1}, ^bn} ->
        {{:m, op}, ([bn] ++ l1) |> Enum.sort()}

      {^an, {{:m, ^op}, l2}} ->
        {{:m, op}, ([an] ++ l2) |> Enum.sort()}

      {^an, ^bn} ->
        {{:m, op}, ([an] ++ [bn]) |> Enum.sort()}
    end
  end

  def norm({op, a, b}) do
    {op, norm(a), norm(b)}
  end

  def norm(other) do
    other
  end

  @doc """
  Change sign of tree
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
    {{:m, :mult}, [-1 | lst]}
  end

  def chsign({{:m, :suma}, lst}) do
    {{:m, :suma},
     Enum.map(lst, fn el ->
       chsign(el)
     end)}
  end

  @doc """
  Change power sign of tree
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
    {:divi, {:numb, 1}, {:suma, a, b}}
  end

  def chpow({:rest, a, b}) do
    {:divi, {:numb, 1}, {:rest, a, b}}
  end

  def chpow({{:m, :mult}, lst}) do
    {{:m, :mult}, Enum.map(lst, fn el -> chpow(el) end)}
  end

  def chpow(suma = {{:m, :suma}, _lst}) do
    {:divi, {:numb, 1}, suma}
  end

  def denorm({{:m, op}, lista}) when op in [:suma, :mult] and is_list(lista) do
    case length(lista) do
      0 ->
        if op == :mult, do: @uno, else: @zero

      1 ->
        denorm(List.first(lista))

      _ ->
        [first, second | tail] = lista

        {res, _} =
          Enum.reduce(tail, {{op, first, second}, :left}, fn el, ac ->
            eld = denorm(el)
            {realac, _} = ac

            case ac do
              {{op, _a, _b}, :left} ->
                {{op, realac, eld}, :right}

              {{op, _a, _b}, :right} ->
                {{op, eld, realac}, :left}
            end
          end)

        res
    end
  end

  def denorm({op, l, r}) do
    {op, denorm(l), denorm(r)}
  end

  def denorm(tree) do
    tree
    # |> IO.inspect(label: "Not possible denorm")
  end

  @doc """
  Sort leafs
  """
  def sort(tree), do: denorm(norm(tree))
end
