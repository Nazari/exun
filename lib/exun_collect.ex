defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Unit
  alias Exun.Simpl
  alias Exun.Eq

  @doc """
  Main collecting function. Try to simplify tree without chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> Simpl.mkrec()
      # |> IO.inspect(label: "make01,mkrec->norm")
      |> Eq.norm()
      # |> IO.inspect(label: "make02, norm->mkrec")
      |> Simpl.mkrec()
      # |> IO.inspect(label: "make02, mkrec->solve_literals")
      |> solve_literals()
      # |> IO.inspect(label: "make03,solve_literals->mkrec")
      |> Simpl.mkrec()
      # |> IO.inspect(label: "make04,mk->denorm")
      |> Eq.denorm()

    if Eq.eq(newtree, tree), do: newtree, else: coll(newtree)
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
        {{:m, op}, [Unit.toSI(Simpl.mkrec({op, collnumb, collunit})) | lst]}
    end
  end

  defp solve_literals(tree) do
    tree
  end
end
