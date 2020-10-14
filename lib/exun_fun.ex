defmodule Exun.Fun do
  @moduledoc """
  Function management, not complete.
  @base and  @compound will be the definitions of external functions.
  """

  @zero {:numb, 0}
  @uno {:numb, 1}

  @doc """
  base is a map that holds functions names and a tupla
   <fname>(F) { <elixir function call reference for numbers>, "Deriv Abstract"}
   For example:

   ln(F) Function name is 'ln'
   {&:math.log/1, "F'x/F"}

   F'x is d(F)/dx

  """
  @base %{
    "ln(F)" => {&:math.log/1, "F'x/F"},
    "sin(F)" => {&:math.sin/1, "F'x*cos(F)"},
    "cos(F)" => {&:math.cos/1, "-F'x*sin(F)"},
    "tan(F)" => {&:math.tan/1, "F'x/cos(F)^2"},
    "acos(F)" => {&:math.acos/1, "-F'x/(1-F^2)^0.5"},
    "asin(F)" => {&:math.asin/1, "F'x/(1-F^2)^0.5"},
    "atan(F)" => {&:math.atan/1, "F'x/(1+F^2)"},
    "sinh(F)" => {&:math.sinh/1, "F'x*cosh(F)"},
    "cosh(F)" => {&:math.cosh/1, "F'x*sinh(F)"},
    "tanh(F)" => {&:math.tanh/1, "F'x/cosh(F)^2"},
    "asinh(F)" => {&:math.asinh/1, "F'x/(F^2+1)^0.5"},
    "acosh(F)" => {&:math.acosh/1, "F'x/(F^2-1)^0.5"},
    "atanh(F)" => {&:math.atanh/1, "F'x/(1-F^2)"}
  }

  @compounds %{
    "sqrt(F)" => "F^0.5"
  }

  def fcall(name, args) do
    # IO.inspect({name, args}, label: "fcall")
    aau = allargs_numbers(args)
    search_name = name <> "(F)"

    cond do
      (bfunc = @base[search_name]) != nil ->
        cond do
          aau -> {:numb, elem(bfunc, 0).(args |> List.first() |> elem(1))}
          true -> {:fcall, name, args}
        end

      (cfunc = @compounds[search_name]) != nil ->
        {ast, _ctx} = Exun.parse(cfunc)
        replace_args(ast, args)

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
    {ast, _ctx} = Exun.parse(txt)

    deriv(ast, x)
    # |> IO.inspect(label: "reduced")
    |> Exun.tostr()
  end

  def deriv(ast, name) when is_tuple(ast) and is_binary(name) do
    der(ast, {:vari, name})
  end

  defp der({:deriv, fun, x1}, x2) do
    der(der(fun, x1), x2)
  end

  defp der({:fcall, name, args}, x) do
    search_name = name <> "(F)"

    cond do
      (bfunc = @base[search_name]) != nil ->
        {ast, _ctx} = Exun.parse(elem(bfunc,1))
        replace_args(ast,args)

      (cfunc = @compounds[search_name]) != nil ->
        {ast, _ctx} = Exun.parse(cfunc)

        replace_args(ast, args)
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
