defmodule Exun.Tree.Eq do
  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    norm(t1) == norm(t2)
  end

  @doc """
  Normalize tree so :mult and :sum are converted to list:
  {:mult,{:mul,a,b},c} -> {{:m,:mult},[a,b,c]}
  """
  def norm({:rest, a, {{:m, :suma}, lst}}) do
    {{:m, :suma}, [a | Enum.map(lst, fn el -> chsign(el) end)]}
  end

  def norm({:rest, {{:m, :suma}, lst}, a}) do
    {{:m, :suma}, lst ++ [chsign(a)]}
  end

  def norm({:divi, a, {{:m, :mult}, lst}}) do
    {{:m, :mult}, [a | Enum.map(lst, fn el -> chpow(el) end)]}
  end

  def norm({:divi, {{:m, :mult}, lst}, a}) do
    {{:m, :mult}, lst ++ [chpow(a)]}
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
    norm({op, norm(a), norm(b)})
  end

  def norm(other) do
    other
  end

  def chsign({:vari, var}) do
    {:mult, {:numb, -1}, {:vari, var}}
  end

  def chsign({:unit, a, b}) do
    {:unit, chsign(a), b}
  end

  def chsign({:elev, a, b}) do
    {:mult, {:numb, -1}, {:elev, a, b}}
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
    {:div, {:numb, 1}, {:suma, a, b}}
  end

  def chpow({:rest, a, b}) do
    {:div, {:numb, 1}, {:rest, a, b}}
  end

  def chpow({{:m, :mult}, lst}) do
    {{:m, :mult}, Enum.map(lst, fn el -> chpow(el) end)}
  end

  def chpow(suma = {{:m, :suma}, _lst}) do
    {:divi, {:numb, 1}, suma}
  end

  @doc """
  Denormalize tree, expand {{:m.op},list} to
  {op,{op,el1,el2},el3}
  """
  def denorm({{:m, op}, lista}) do
    Enum.reduce(lista, {op, nil, nil}, fn el, ac ->
      eld = denorm(el)

      case ac do
        {op, nil, nil} -> {op, eld, nil}
        {op, a, nil} -> {op, a, eld}
        {op, _a, _b} -> {op, ac, eld}
      end
    end)
  end

  def denorm({op, l, r}) do
    {op, denorm(l), denorm(r)}
  end

  def denorm(tree) do
    tree
  end

  def sort(tree), do: denorm(norm(tree))
end
