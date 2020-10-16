defmodule Exun.Math do
  @moduledoc """
  Simple transformation of a tree
  """
    @doc """
  Change sign of AST
  """

  @uno {:numb, 1}
  @muno {:numb, -1}

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

  def chsign(ast) do
    {:mult, @muno, ast}
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

end
