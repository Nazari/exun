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
    {txt, _, d} = show(tree)

    cond do
      String.starts_with?(txt, "/") -> "1" <> txt
      d -> "1/" <> txt
      true -> txt
    end
  end

  def show({:numb, n}),
    do: {to_string(n), 200, false}

  def show({:vari, v}),
    do: {v, 200, false}

  def show({:unit, n, m}) do
    {t, p, _} = show(n)
    {tt, _, _} = show(m)
    {"#{t}[#{tt}]", p, false}
  end

  def show({:minus, a}) do
    {t, p, _} = show(a)

    if p < 200,
      do: {"-(#{t})", p, false},
      else: {"-#{t}", p, false}
  end

  def show({:elev, b, {:numb, nn}}) when nn < 0 do
    {t, p, _} = show({:elev, b, {:numb, -nn}})
    {t, p, true}
  end

  def show({:elev, b, {:minus, a}}) do
    {t, p, _} = show({:elev, b, a})
    {t, p, true}
  end

  def show({:elev, b, e}) do
    {tb, pb, _} = show(b)
    {te, pe, _} = show(e)

    cond do
      pb < 200 and e == {:numb, 1} -> {"(#{tb})", pb, false}
      e == {:numb, 1} -> {"#{tb}", pb, false}
      pe < 200 -> {"#{tb}^(#{te})", pb, false}
      true -> {"#{tb}^#{te}", pb, false}
    end
  end

  def show({:deriv, f, v}) do
    {tf, pf, _} = show(f)
    {tv, _, _} = show(v)

    if pf < 200,
      do: {"(#{tf})'#{tv}", pf, false},
      else: {"#{tf}'#{tv}", pf, false}
  end

  def show({:integ, f, v}) do
    {tf, pf, _} = show(f)
    {tv, _, _} = show(v)

    if pf < 200,
      do: {"($#{tf},#{tv})", pf, false},
      else: {"$#{tf},#{tv}", pf, false}
  end

  def show({:fcall, n, a}) do
    targs =
      Enum.reduce(a, [], fn arg, ltxt ->
        {t, _, _} = show(arg)
        [t | ltxt]
      end)
      |> Enum.reverse()
      |> Enum.join(",")

    {"#{n}(#{targs})", 200, false}
  end

  def show({{:m, op}, lst}) do
    lst = Enum.sort(lst, &smm(&1, &2))
    {opt, pri} = if op == :suma, do: {"+", 50}, else: {"*", 90}

    texto =
      Enum.map(lst, fn opand ->
        {t, p, d} = show(opand)

        cond do
          d and op == :suma -> "1/#{t}"
          d and op == :mult -> "/#{t}"
          p < pri -> "(#{t})"
          true -> "#{t}"
        end
      end)
      # |> IO.inspect(label: "map")
      |> Enum.sort(&sop/2)
      |> Enum.join(opt)
      |> String.replace("*/", "/")

    {texto, pri, false}
  end

  defp sop(a, _b) do
    if String.starts_with?(a, "/"), do: false, else: true
  end
end
