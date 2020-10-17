defmodule Exun.Integral do
  import Exun.Collect
  import Exun.Fun
  import Exun

  @zero {:numb, 0}
  @uno {:numb, 1}
  @dos {:numb, 2}
  @moduledoc """
  Try to integrate function
  """
  def integ(@zero, _), do: @uno
  def integ(n = {:numb, _}, v), do: {:mult, n, v}
  def integ(v = {:vari, _}, v), do: {:mult, {:numb, 0.5}, {:elev, v, @dos}}
  def integ({:suma, a, b}, v), do: {:suma, integ(a, v), integ(b, v)}
  def integ({:rest, a, b}, v), do: {:rest, integ(a, v), integ(b, v)}
  def integ({:mult, n = {:numb, _}, a}, v), do: {:mult, n, integ(a, v)}
  def integ({:mult, a, n = {:numb, _}}, v), do: {:mult, n, integ(a, v)}
  def integ({:deriv, f, x}, x), do: f

  def integ({:elev, {:vari, v}, expon = {:numb, _}}, {:vari, v}) do
    newexp = {:suma, expon, @uno}
    {:divi, {:elev, {:vari, v}, newexp}, newexp}
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

  # Fallthrough
  def integ(c, v) do
    {:integ, c, v}
  end

  def parts({:mult, l, r}, x) do
    # u=l, dv=r
    v1 = coll({:integ, r, x})
    sr1 = coll({:integ, {:mult, {:deriv, l, x}, v1}, x})
    # u=r, dv=l
    v2 = coll({:integ, l, x})
    sr2 = coll({:integ, {:mult, {:deriv, r, x}, v2}, x})

    case {check_parts(v1, sr1), check_parts(v2, sr2)} do
      {:ok, _} -> doparts(l, v1, sr1)
      {_, :ok} -> doparts(r, v2, sr2)
      _ -> :fail
    end
  end

  def parts(_, _) do
    {:fail}
  end

  def check_parts(v, sr) do
    {op1, _, _} = v
    {op2, _, _} = sr

    case {op1, op2} do
      {:integ, _} -> :fail
      {_, :integ} -> :fail
      _ -> :ok
    end
  end

  def doparts(u, v, sr) do
    {:rest, {:mult, u, v}, sr}
  end
end
