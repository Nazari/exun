defmodule Exun.Eq do
  alias Exun.Math

  @zero {:numb, 0}
  @uno {:numb, 1}

  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    norm(t1) == norm(t2)
  end
  @moduledoc """
  Compares two expressions
  """
  @doc """
  Normalize tree so :mult and :suma are converted to list:
  {:mult,{:mult,a,b},c} -> {{:m,:mult},[a,b,c]}
  """

  def norm({:divi, a, b}) do
    norm({{:m, :mult}, [norm(a), {:elev, norm(b), {:numb, -1}}] |> Enum.sort()})
  end

  def norm({:rest, a, b}) do
    norm({{:m, :suma}, [norm(a), Math.chsign(norm(b))] |> Enum.sort()})
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
    {op, denorm(l), denorm(r)}
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
