defmodule Exun.UI do
  alias Exun.Eq
  alias Exun.Collect

  @defop %{
    :deriv => {110, "'"},
    :integ => {110, "$"},
    :elev => {100, "^"},
    :mult => {90, "*"},
    :divi => {90, "/"},
    :suma => {50, "+"},
    :rest => {50, "-"},
    :equal => {20, "="},
    :numb => {200, nil},
    :unit => {200, nil},
    :vari => {200, nil},
    :fcall => {200, nil},
  }
  @moduledoc """
  Parses a tree and transform to string in User Readable form
  """
    @doc ~S"""
  Translate tree to human readable math expression:
  ```
    iex(1)> {_tree, _deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
    {{:divi,
    {:mult, {:numb, 4}, {:elev, {:vari, "x"}, {:suma, {:vari, "y"}, {:numb, 1}}}},
    {:vari, "z"}}, %{{:vari, "z"} => {:suma, {:vari, "y"}, {:numb, 1}}}}

  ```
  """
  def tostr(tree) do
    tree
    # |> IO.inspect(label: "tostr1,orig")
    |> Eq.denorm()
    # |> IO.inspect(label: "tostr2,denorm")
    |> its()
    |> aesthetic()
  end

  defp aesthetic(str) do
    newstr = aest(str)
    if str == newstr, do: newstr, else: aest(newstr)
  end

  defp aest(str) do
    %{"+-" => "-", "-+" => "-", "--" => "+", "++" => "+"}
    |> Enum.reduce(str, fn {k, v}, str -> String.replace(str, k, v) end)
  end

  defp its({:mult, {:numb, -1}, a}), do: "-(#{its(a)})"
  defp its({:mult, a, {:numb, -1}}), do: "-(#{its(a)})"
  defp its({:mult, {:divi, {:numb, 1}, a}, b}), do: its({:divi, b, a})
  defp its({:mult, a, {:elev, b, {:numb, n}}}) when n < 0, do: its({:divi, a, {:elev, b, {:numb, -n}}})
  defp its({:mult, {:elev, b, {:numb, n}}, a}) when n < 0, do: its({:divi, a, {:elev, b, {:numb, -n}}})
  defp its({:mult, b, {:divi, {:numb, 1}, a}}), do: its({:divi, b, a})
  defp its({:vari, var}), do: var
  defp its({:elev, a, {:numb, 1}}), do: its(a)
  defp its({:elev, a, {:numb, -1}}), do: its({:divi, {:numb, 1}, a})

  defp its({:fcall, name, args}) when is_list(args) do
    name <>
      "(" <>
      Enum.reduce(args, "", fn el, ac ->
        case ac do
          "" -> its(el)
          _ -> ac <> ", " <> its(el)
        end
      end) <> ")"
  end

  defp its({:unit, n, tree}), do: its(n) <> "[" <> its(Collect.coll(tree)) <> "]"
  defp its({:numb, n}), do: if n == floor(n), do: to_string(floor(n)), else: to_string(n)
  defp its({:deriv, a, {:vari, x}}), do: its(a) <> "'" <> x
  defp its({:integ, f, var}), do: "$"<>its(f)<>","<>its(var)

  defp its({op, l, r}) do
    # IO.inspect([op,l,r])
    {hpri, hstr} = @defop[op]
    {lpri, _} = @defop[l |> elem(0)]
    {rpri, _} = @defop[r |> elem(0)]

    ltxt = its(l)
    rtxt = its(r)
    conctostr(hpri, hstr, lpri, ltxt, rpri, rtxt)
  end

  defp conctostr(hpri, hstr, lpri, ltxt, rpri, rtxt) do
    cond do
      hpri > lpri and hpri > rpri ->
        "(" <> ltxt <> ")" <> hstr <> "(" <> rtxt <> ")"

      hpri > lpri ->
        "(" <> ltxt <> ")" <> hstr <> rtxt

      hpri > rpri ->
        ltxt <> hstr <> "(" <> rtxt <> ")"

      true ->
        ltxt <> hstr <> rtxt
    end
  end

end
