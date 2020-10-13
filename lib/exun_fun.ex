defmodule Exun.Fun do
  @moduledoc """
  Function management, not complete.
  @base and  @compound will be the definitions of external functions.
  """
  import Exun

  @zero {:numb, 0}
  @uno {:numb, 1}

  @base %{
    "sin(F)" => {&:math.sin/1, "F'x*cos(F)"},
    "cos(F)" => {&:math.cos/1, "-F'x*sin(F)"},
    "ln(F)" => {&:math.log/1, "F'x/F"}
  }

  @compounds %{
    "tan(F)" => "sin(F)/cos(F)"
  }

  def fcall(name, args) do
    # IO.inspect({name, args}, label: "fcall")
    aau = allargs_numbers(args)
    search_name = name <> "(F)"

    cond do
      (b = @base[search_name]) != nil ->
        cond do
          aau -> {:numb, elem(b, 0).(args |> List.first() |> elem(1))}
          true -> {:fcall, name, args}
        end

      (c = @compounds[search_name]) != nil ->
        c
        |> Exun.parse()
        |> replace_args(args)

      true ->
        {:fcall, name, args}
    end
  end

  defp replace_args(ast, args) do
    case ast do
      {:vari, "F"} ->
        args |> List.first()

      {:fcall, name, [{:vari, "F"}]} ->
        {:fcall, name, args}

      {:unit, uv, ut} ->
        {:unit, replace_args(uv, args), ut}

      {op, l, r} ->
        {op, replace_args(l, args), replace_args(r, args)}

      _ ->
        ast
    end
  end

  defp allargs_numbers(args) do
    Enum.reduce(args, true, fn el, ac ->
      ac and elem(el, 0) == :numb
    end)
  end

  @doc """
  Derive function txt for variable x, return string
  """
  def deriv(txt, x) when is_binary(txt) and is_binary(x) do
    txt
    |> parse()
    |> deriv(x)
    # |> IO.inspect(label: "reduced")
    |> tostr()
  end

  def deriv(ast, name) when is_tuple(ast) and is_binary(name) do
    ast
    |> der({:vari, name})
  end

  defp der({:fcall, name, args}, x) do
    search_name = name <> "(F)"

    cond do
      (b = @base[search_name]) != nil ->
        b
        |> elem(1)
        |> parse()
        # |> IO.inspect(label: "der parsed")
        |> replace_args(args)

      # |> IO.inspect(label: "replaced")

      (c = @compounds[search_name]) != nil ->
        c
        |> parse
        |> replace_args(args)
        |> der(x)

      true ->
        {:deriv, {:fcall, name, args}, x}
    end
  end

  defp der({:numb, _}, _x),
    do: @zero

  defp der({:unit, _uv, _ut}, _x),
    do: @zero

  defp der({:vari, var}, {:vari, x}),
    do: if(var == x, do: @uno, else: @zero)

  defp der({:suma, a, b}, x),
    do: {:suma, der(a, x), der(b, x)}

  defp der({:rest, a, b}, x),
    do: {:rest, der(a, x), der(b, x)}

  defp der({:mult, a, b}, x),
    do: {:suma, {:mult, der(a, x), b}, {:mult, a, der(b, x)}}

  defp der({:divi, a, b}, x),
    do: {:divi, {:rest, {:mult, b, der(a, x)}, {:mult, der(b, x), a}}, {:elev, b, {:numb, 2}}}

  defp der(y = {:elev, f, g}, x),
    do:
      {:mult, y,
       {:suma, {:mult, der(g, x), {:fcall, "ln", [f]}}, {:mult, g, {:divi, der(f, x), f}}}}
end
