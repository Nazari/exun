defmodule Exun.Integral do

  import Exun.Fun
  import Exun.Math
  import Exun

  @zero {:numb, 0}
  @uno {:numb, 1}
  @dos {:numb, 2}
  @moduledoc """
  Try to integrate function
  """
  @doc """
  Integrate function for var v. Expect an AST and return an AST.
  """
  def integ(@zero, _), do: @uno
  def integ(n = {:numb, _}, v), do: mult( n, v)
  def integ(v = {:vari, _}, v), do: mult({:numb, 0.5}, {:elev, v, @dos})
  def integ({:deriv, f, x}, x), do: f
  def integ({:vari, a}, v), do: mult({:vari, a}, v)

  def integ({:elev, {:vari, v}, expon = {:numb, _}}, {:vari, v}) do
    newexp = suma(expon, @uno)
    mult({:elev, {:vari, v}, newexp},chpow(newexp))
  end

  def integ({:fcall, "sin", [v = {:vari, x}]}, v),
    do: parse_text("-cos(" <> x <> ")")

  def integ({:fcall, "cos", [v = {:vari, x}]}, v),
    do: parse_text("sin(" <> x <> ")")

  def integ({:fcall, "tan", [v = {:vari, x}]}, v),
    do: parse_text("-ln(cos(" <> x <> "))")

  def integ({:fcall, "asin", [v = {:vari, x}]}, v) do
    txt = "x*asin(x)+(1-x)^0.5*(1+x)^0.5"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "acos", [v = {:vari, x}]}, v) do
    txt = "x*acos(x)-(1-x)^0.5*(1+x)^0.5"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "atan", [v = {:vari, x}]}, v) do
    txt = "-ln(x^2+1)-2*x*atan(x)/2"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "sinh", [v = {:vari, _x}]}, v) do
    {:fcall, "cosh", [v]}
  end

  def integ({:fcall, "cosh", [v = {:vari, _x}]}, v) do
    {:fcall, "sinh", [v]}
  end

  def integ({:fcall, "tanh", [v = {:vari, x}]}, v) do
    txt = "ln(cosh(x))"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "asinh", [v = {:vari, x}]}, v) do
    txt = "x*asinh(x)-(x^2+1)^0.5"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "acosh", [v = {:vari, x}]}, v) do
    txt = "x*acosh(x)-(x-1)^0.5*(x+1)^0.5"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "atanh", [v = {:vari, x}]}, v) do
    txt = "(ln(1+x)+2*x*atanh(x)+ln(1-x))/2"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, name, args}, v) do
    case base()[name <> "(F)"] do
      {_, _, val} when val != nil ->
        {ast, _} = Exun.parse(val)
        replace_args_internal(ast, args, v)

      _ ->
        {:integ, {:fcall, name, args}, v}
    end
  end

  def integ({{:m, :suma}, lst}, v) do
    {{:m, :suma}, Enum.map(lst, &integ(&1, v))}
  end

  def integ({{:m,:mult},_},_v), do: throw "Product integral not yet"

  # Fallthrough
  def integ(c, v) do
    {:integ, c, v}
  end

end
