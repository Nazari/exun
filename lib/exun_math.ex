defmodule Exun.Math do
  import Exun.Eq

  @moduledoc """
  Simple transformation of a tree
  """
  @doc """
  Change sign of AST
  """
  import Exun.Collect
  import Exun.Eq

  def chsign({:minus, a}), do: a
  def chsign({:unit, a, b}), do: {:unit, chsign(a), b}
  def chsign({:numb, n}), do: {:numb, -n}

  def chsign({{:m, :mult}, lst}) do
    {
      {:m, :mult},
      List.replace_at(
        lst,
        0,
        chsign(List.first(lst))
        |> Enum.sort(&smm(&1, &2))
      )
    }
  end

  def chsign({{:m, :suma}, lst}) do
    {{:m, :suma},
     Enum.map(lst, fn el ->
       chsign(el)
     end)
     |> Enum.sort(&smm(&1, &2))}
  end

  def chsign(ast), do: {:minus, ast}

  @doc """
  Change power sign of AST (expon * -1 or 1/tree)
  """
  def chpow({:vari, var}), do: coll({:elev, {:vari, var}, {:numb, -1}})
  def chpow({:unit, a, b}), do: coll({:unit, chpow(a), chpow(b)})
  def chpow({:elev, a, b}), do: coll({:elev, a, chsign(b)})
  def chpow({:numb, n}), do: {:numb, 1 / n}

  def chpow({{:m, :mult}, lst}) do
    coll({
      {:m, :mult},
      Enum.map(lst, fn el -> chpow(el) end)
    })
  end

  def chpow(s = {{:m, :suma}, _}), do: {:elev, s, {:numb, -1}}
  def chpow(ast), do: {:elev, ast, {:numb, -1}}
end
