defmodule Exun.UI do
  import Exun.Eq

  @moduledoc """
  Parses a tree and transform to string in User Readable form
  """
  @doc ~S"""
  Translate tree to human readable math expression:
  ```
    iex(1)> {_tree, _deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
  {{{:m, :mult},
  [
    {:numb, 4},
    {:elev, {:vari, "x"}, {{:m, :suma}, [numb: 1, vari: "y"]}},
    {:elev, {:vari, "z"}, {:numb, -1}}
  ]}, %{{:vari, "z"} => {{:m, :suma}, [numb: 1, vari: "y"]}}}
  ```
  """
  def tostr(tree) do
    {t, _, d, s} =
      show(tree)
      #|> IO.inspect(label: "root show")

    cond do
      d and s -> "-1/#{t}"
      d and !s -> "1/#{t}"
      !d and s -> "-#{t}"
      !d and !s -> t
    end
  end

  @doc """
  Prints in human readable form an AST.
  Each returns the text converted, the prioriti of the node, a flag
  indicating if must be included in denominator of a division, and
  another flag indicatin if must be preceded with a sign - before expression
  """
  def show({:numb, n}),
    do: {to_string(n), 200, false, false}

  def show({:vari, v}),
    do: {v, 200, false, false}

  def show({:unit, n, m}) do
    {t, p, _, s} = show(n)
    tt = tostr(m)
    {"#{t}[#{tt}]", p, false, s}
  end

  def show({:minus, a}) do
    {t, p, d, s} = show(a)
    # Xor sign with returned sign s
    ns = if s, do: false, else: true
    {withpar(p, 200, t), p, d, ns}
  end

  def show({:elev, b, {:numb, nn}}) when nn < 0 do
    {t, p, d, s} = show({:elev, b, {:numb, -nn}})
    # Xor denomi with returned denomi
    nd = if d, do: false, else: true
    {t, p, nd, s}
  end

  def show({:elev, b, {:minus, a}}) do
    {t, p, d, s} = show({:elev, b, a})
    # Xor denomi with returned denomi
    nd = if d, do: false, else: true
    {t, p, nd, s}
  end

  def show({:elev, b, e}) do
    {tb, pb, d, s} = show(b)
    {_, pe, _, _} = show(e)
    te = tostr(e)
    tbase = withpar(pb, 200, tb)
    texpo = withpar(pe, 200, te)

    cond do
      e == {:numb, 1} -> {"#{tbase}", pb, d, s}
      true -> {"#{tbase}^#{texpo}", pb, d, s}
    end
  end

  def show({:deriv, f, v}) do
    {tf, pf, d, s} = show(f)
    tv = tostr(v)

    tderi = withpar(pf, 200, tf)
    {"#{tderi}'#{tv}", pf, d, s}
  end

  def show({:integ, f, v}) do
    {tf, pf, d, s} = show(f)
    tv = tostr(v)
    tfinteg = withpar(pf, 200, tf)
    {"$#{tfinteg},#{tv}", pf, d, s}
  end

  def show({:fcall, n, a}) do
    targs =
      Enum.reduce(a, [], fn arg, ltxt ->
        t = tostr(arg)
        [t | ltxt]
      end)
      |> Enum.reverse()
      |> Enum.join(",")

    {"#{n}(#{targs})", 200, false, false}
  end

  def show({{:m, op}, lst}) do
    lst = Enum.sort(lst, &smm(&1, &2))
    {_opt, pri} = if op == :suma, do: {"+", 50}, else: {"*", 90}

    texto =
      Enum.map(lst, &show(&1))
      |> Enum.sort(&sop/2)
      |> Enum.reduce("", fn {t, p, d, s}, atxt ->
        wp = withpar(p, pri, t)

        if atxt == "" do
          if s, do: "-" <> wp, else: wp
        else
          suma = op == :suma

          cond do
            d and s and suma -> "#{atxt}-1/#{wp}"
            d and s and !suma -> "#{atxt}/-#{wp}"
            d and !s and suma -> "#{atxt}+1/#{wp}"
            d and !s and !suma -> "#{atxt}/#{wp}"
            !d and s and suma -> "#{atxt}-#{wp}"
            !d and s and !suma -> "#{atxt}*-#{wp}"
            !d and !s and suma -> "#{atxt}+#{wp}"
            !d and !s and !suma -> "#{atxt}*#{wp}"
          end
        end
      end)

    {texto, pri, false, false}
  end

  def sop({_, p1, d1, s1}, {_, p2, d2, s2}) do
    cond do
      d1 and !d2 -> false
      !d1 and d2 -> true
      s1 and !s2 -> false
      !s1 and s2 -> true
      true -> p1 > p2
    end
  end

  defp withpar(p, pmax, t), do: if p < pmax, do: "(#{t})", else: "#{t}"
end
