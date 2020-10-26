defmodule Exun.Fun do
  @moduledoc """
  Function management.
  @base and  @compound are the definitions of external functions.
  """

  @doc """
  base is a map that holds functions names and a tupla
   <fname>(F) { <elixir function call reference for numbers>, "Deriv Abstract"}
   For example:

   ln(F) Function name is 'ln'
   {&:math.log/1, "F'x/F"}

   F'x is d(F)/dx

  """
  # name => Numeric implementation, Derivate, Integrate
  def base,
    do: %{
      "ln(F)" => {&:math.log/1, "F'x/F", "x*ln(F)-$(x/F),x"},
      "sin(F)" => {&:math.sin/1, "F'x*cos(F)", nil},
      "cos(F)" => {&:math.cos/1, "-F'x*sin(F)", nil},
      "tan(F)" => {&:math.tan/1, "F'x/cos(F)^2", nil},
      "acos(F)" => {&:math.acos/1, "-F'x/(1-F^2)^0.5", nil},
      "asin(F)" => {&:math.asin/1, "F'x/(1-F^2)^0.5", nil},
      "atan(F)" => {&:math.atan/1, "F'x/(1+F^2)", nil},
      "sinh(F)" => {&:math.sinh/1, "F'x*cosh(F)", nil},
      "cosh(F)" => {&:math.cosh/1, "F'x*sinh(F)", nil},
      "tanh(F)" => {&:math.tanh/1, "F'x/cosh(F)^2", nil},
      "asinh(F)" => {&:math.asinh/1, "F'x/(F^2+1)^0.5", nil},
      "acosh(F)" => {&:math.acosh/1, "F'x/(F^2-1)^0.5", nil},
      "atanh(F)" => {&:math.atanh/1, "F'x/(1-F^2)", nil}
    }

  def compounds,
    do: %{
      "sqrt(F)" => "F^0.5",
      "tan(F)" => "sin(F)/cos(F)"
    }

  def fcall(name, args) do
    # IO.inspect({name, args}, label: "fcall")
    aau = allargs_numbers(args)

    cond do
      (bfunc = base()[name <> "(F)"]) != nil ->
        cond do
          aau -> {:numb, elem(bfunc, 0).(args |> List.first() |> elem(1))}
          true -> {:fcall, name, args}
        end

      (cfunc = compounds()[name <> "(F)"]) != nil ->
        {ast, _ctx} = Exun.parse(cfunc)
        replace_args_internal(ast, args, {:vari, "x"})

      true ->
        {:fcall, name, args}
    end
  end

  def replace_args_internal(ast, args, vari) do
    case ast do
      {:vari, "F"} ->
        args |> List.first()

      {:vari, "x"} ->
        vari

      {:fcall, name, [{:vari, "F"}]} ->
        {:fcall, name, args}

      {:unit, uv, ut} ->
        {:unit, replace_args_internal(uv, args, vari), ut}

      {{:m, op}, lst} ->
        {{:m, op}, Enum.map(lst, &replace_args_internal(&1, args, vari))}

      {:minus, a} ->
        {:minus, replace_args_internal(a, args, vari)}

      {op, l, r} ->
        {op, replace_args_internal(l, args, vari), replace_args_internal(r, args, vari)}

      _ ->
        ast
    end
  end

  defp allargs_numbers(args) do
    Enum.reduce(args, true, fn el, ac ->
      ac and elem(el, 0) == :numb
    end)
  end

  def mult(a, b), do: mcompose(:mult, a, b)
  def divi(a, b), do: mult(a, Exun.Math.chpow(b))
  def suma(a, b), do: mcompose(:suma, a, b)
  def rest(a, b), do: suma(a, Exun.Math.chsign(b))

  def mcompose(op, a, b) do
    case {a, b} do
      {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
        Exun.Collect.coll({{:m, op}, l1 ++ l2})

      {{{:m, ^op}, l1}, _} ->
        Exun.Collect.coll({{:m, op}, [b | l1]})

      {_, {{:m, ^op}, l2}} ->
        Exun.Collect.coll({{:m, op}, [a | l2]})

      _ ->
        Exun.Collect.coll({{:m, op}, [a, b]})
    end
  end
end
