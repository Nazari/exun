defmodule Exun.Math do
  alias Exun.Collect
  alias Exun.Eq
  alias Exun.Math

  @muno {:numb, -1, 1}

  @moduledoc """
  Simple transformation of a tree
  """
  @doc """
  Change sign of AST
  """

  def chsign({:minus, a}), do: a
  def chsign({:unit, a, b}), do: {:unit, chsign(a), b}
  def chsign({:numb, n, d}), do: {:numb, -n, d}

  def chsign({{:m, :mult}, lst}) do
    felem = List.first(lst)

    new_list =
      List.replace_at(lst, 0, Math.chsign(felem))
      |> Enum.sort(&Eq.smm(&1, &2))

    {{:m, :mult}, new_list}
  end

  def chsign({{:m, :suma}, lst}) do
    {{:m, :suma},
     Enum.map(lst, fn el ->
       chsign(el)
     end)
     |> Enum.sort(&Eq.smm(&1, &2))}
  end

  def chsign(ast), do: {:minus, ast}

  @doc """
  Change power sign of AST (expon * -1 or 1/tree)
  """
  def chpow({:vari, var}), do: Collect.coll({:elev, {:vari, var}, @muno})
  def chpow({:unit, a, b}), do: Collect.coll({:unit, chpow(a), chpow(b)})
  def chpow({:elev, a, b}), do: Collect.coll({:elev, a, chsign(b)})
  def chpow({:numb, n, d}), do: {:numb, d, n}

  def chpow({{:m, :mult}, lst}) do
    Collect.coll({
      {:m, :mult},
      Enum.map(lst, fn el -> chpow(el) end)
    })
  end

  def chpow(s = {{:m, :suma}, _}), do: {:elev, s, @muno}
  def chpow(ast), do: {:elev, ast, @muno}

  def signof({:numb,a,b}) do
    r=a/b
    cond do
      r<0 -> :neg
      r>0 -> :pos
      true -> :zero
    end
  end

  defp mknum(n, d) do
    if floor(n)==n and floor(d)==d do
      n=floor(n)
      d=floor(d)
      mcd = Integer.gcd(n, d)
      {:numb, n / mcd, d / mcd}
    else
      {:numb, n, d}
    end
  end

  @doc """
  For convenience, creates ast {{:m,:mult},[a,b]}
  """
  def mult({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * n2, d1 * d2)
  def mult(a, b), do: mcompose(:mult, a, b)

  @doc """
  For convenience, creates ast {{:m,:mult},[a,b^-1]}
  """
  def divi({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2, d1 * n2)
  def divi(a, b), do: mult(a, Exun.Math.chpow(b))

  @doc """
  For convenience, creates ast {{:m,:suma},[a,b]}
  """
  def suma({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2 + n2 * d1, d1 * d2)
  def suma(a, b), do: mcompose(:suma, a, b)

  @doc """
  For convenience, creates ast {{:m,:mult},[a,-b]}
  """
  def rest({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2 - n2 * d1, d1 * d2)
  def rest(a, b), do: suma(a, Exun.Math.chsign(b))

  @doc """
  For convenience, creates ast {:elev,a,b}
  """
  def elev({:numb, n1, d1}, {:numb, n2, d2}),
    do: {:numb, :math.pow(n1,n2/d2), :math.pow(d1,n2/d2)}

  def elev(a, b), do: {:elev, a, b}

  def minus(a), do: {:minus, a}

  def ln(a), do: {:fcall, "ln", [a]}
  def exp(a, b), do: {:fcall, "exp", [a, b]}

  defp mcompose(op, a, b) do
    Collect.coll(
      case {a, b} do
        {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
          {{:m, op}, l1 ++ l2}

        {{{:m, ^op}, l1}, _} ->
          {{:m, op}, [b | l1]}

        {_, {{:m, ^op}, l2}} ->
          {{:m, op}, [a | l2]}

        _ ->
          {{:m, op}, [a, b]}
      end
    )
  end
end
