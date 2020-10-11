defmodule Exun.Collect do
  alias Exun.Unit

  @zero {:numb, 0}
  @uno {:numb, 1}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @infinite {:numb, :infinite}
  @invalid_unit_operation "Inconsistent unit operation"

  def make(tree) do
    newtree =
      tree
      #|> IO.inspect(label: "make00, orig->mkrec")
      |> mkrec()
      #|> IO.inspect(label: "make01,mkrec->norm")
      |> norm()
      #|> IO.inspect(label: "make02, norm->solve_literals")
      |> solve_literals()
      #|> IO.inspect(label: "make03,solve_literals->mkrec")
      |> mkrec()
      #|> IO.inspect(label: "make04,mk->denorm")
      |> denorm()

    if Exun.eq(newtree, tree), do: newtree, else: make(newtree)
  end

  def mkrec(tree) do
    ntree = mk(tree)

    if Exun.eq(ntree, tree),
      do: ntree,
      else: mkrec(ntree)
  end

  @doc """
  Simplify literals, reduce to one number and one unit when possible
  """
  def solve_literals({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
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

  def solve_literals(tree) do
    tree
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

  @doc """
  Returns commom base for two trees. Used for collect {:m,:suma}
  """
  def cbs(op, a, a) when op in [:suma, :mult] do
    {:ok, @uno}
  end

  def cbs(op, {:elev, a, e1}, {:elev, a, e2}) do
    case op do
      :suma -> {:err, nil}
      :mult -> {:ok, mk({:divi, e2, e1})}
    end
  end

  def cbs(op, a, {:elev, a, b}) do
    case op do
      :suma -> {:ok, mk({:elev, a, mk({:rest, b, @uno})})}
      :mult -> {:ok, b}
      _ -> {:err, nil}
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
  def mk({:suma, a, @zero}), do: mk(a)
  def mk({:suma, @zero, a}), do: mk(a)
  def mk({:mult, _, @zero}), do: @zero
  def mk({:mult, @zero, _}), do: @zero
  def mk({:mult, @uno, a}), do: mk(a)
  def mk({:mult, a, @uno}), do: mk(a)
  def mk({:mult, a, a}), do: {:elev, mk(a), @dos}
  def mk({:mult, a, {:divi, @uno, b}}), do: {:mult, mk(a), mk(b)}
  def mk({:mult, {:elev, a, e1}, {:elev, a, e2}}), do: {:elev, mk(a), mk({:suma, mk(e1), mk(e2)})}
  def mk({:divi, _, @zero}), do: @infinite
  def mk({:divi, @zero, _}), do: @zero
  def mk({:divi, a, @uno}), do: mk(a)
  def mk({:divi, a, a}), do: @uno
  def mk({:elev, _, @zero}), do: @uno
  def mk({:elev, a, @uno}), do: mk(a)
  def mk({:elev, @uno, _}), do: @uno
  def mk({:elev, {:elev, base, e1}, e2}), do: {:elev, mk(base), mk({:mult, e1, e2})}
  def mk({:unit, val, {:numb, 1}}), do: mk(val)
  def mk({:unit, val, ut}), do: Unit.toSI({:unit, val, ut})
  def mk({:numb, n}), do: if(floor(n) == n, do: {:numb, floor(n)}, else: {:numb, n})

  def mk({{:m, op}, lst}) when op in [:suma, :mult] and is_list(lst) do
    # Remove zeroes or ones, 0+any=any, 1*any=any
    unity = if op == :suma, do: @zero, else: @uno
    lst = Enum.reject(lst, &(&1 == unity))
    # Simplify each
    # lst = lst |> Enum.map(&make(&1))

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

  def mk({:elev, {:numb, base}, {:numb, exp}}) do
    {:numb, :math.pow(base, exp)}
  end

  # number, unit
  def mk({:suma, {:numb, _}, {:unit, _, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:rest, {:numb, _}, {:unit, _, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:mult, {:numb, n1}, {:unit, n2, a}}) do
    {:unit, mk({:mult, {:numb, n1}, n2}), mk(a)}
  end

  def mk({:divi, {:numb, n1}, {:unit, n2, a}}) do
    {:unit, mk({:divi, {:numb, n1}, n2}), chpow(mk(a))}
  end

  # unit, number
  def mk({:suma, {:unit, _, _}, {:numb, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:rest, {:unit, _, _}, {:numb, _}}) do
    throw(@invalid_unit_operation)
  end

  def mk({:mult, {:unit, n2, a}, n = {:numb, _n1}}) do
    {:unit, mk({:mult, n2, n}), mk(a)}
  end

  def mk({:divi, {:unit, n1, a}, n = {:numb, _n2}}) do
    {:unit, mk({:divi, n1, n}), mk(a)}
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
    {:unit, mk({:mult, n1, n2}), mk({:mult, a1, a2})}
  end

  def mk({:divi, {:unit, n1, a1}, {:unit, n2, a2}}) do
    {:unit, mk({:divi, n1, n2}), mk({:divi, a1, a2})}
  end

  def mk({op, a, b}) do
    {op, mk(a), mk(b)}
  end

  # Fallthrough
  def mk(tree) do
    tree
  end

  def scoll(tree) do
    tree
  end

  @doc """
  Normalize tree so :mult and :suma are converted to list:
  {:mult,{:mult,a,b},c} -> {{:m,:mult},[a,b,c]}
  """

  def norm({:divi, a, b}) do
    norm({{:m, :mult}, [norm(a), {:elev, norm(b), {:numb, -1}}]})
  end

  def norm({:rest, a, b}) do
    norm({{:m, :suma}, [norm(a), chsign(norm(b))]})
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
    lista = Enum.map(lista,&denorm/1)

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
