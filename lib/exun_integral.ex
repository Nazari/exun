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
  def integ({:minus, @zero}, _), do: {:numb, -1}
  def integ({:minus, a}, v), do: {:minus, integ(a, v)}
  def integ(n = {:numb, _}, v), do: mult(n, v)
  def integ(v = {:vari, _}, v), do: mult({:numb, 0.5}, {:elev, v, @dos})
  def integ({:deriv, f, x}, x), do: f
  def integ({:vari, a}, v), do: mult({:vari, a}, v)

  def integ({:elev, {:vari, v}, expon = {:numb, _}}, {:vari, v}) do
    newexp = suma(expon, @uno)
    mult({:elev, {:vari, v}, newexp}, chpow(newexp))
  end

  def integ({:fcall, "sin", [v = {:vari, x}]}, v),
    do: parse_text("-cos(" <> x <> ")")

  def integ({:fcall, "cos", [v = {:vari, x}]}, v),
    do: parse_text("sin(" <> x <> ")")

  def integ({:fcall, "tan", [v = {:vari, x}]}, v),
    do: parse_text("-ln(cos(" <> x <> "))")

  def integ({:fcall, "asin", [v = {:vari, x}]}, v) do
    txt = "x*asin(x)+(1-x^2)^0.5"
    parse_text(txt |> String.replace("x", x))
  end

  def integ({:fcall, "acos", [v = {:vari, x}]}, v) do
    txt = "x*acos(x)-(1-x^2)^0.5"
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

  def integ(aexp = {{:m, :mult}, _}, v) do
    # Three methods for now
    cond do
      try_poly = integ_poly(aexp, v) ->
        try_poly

      try_udu = integ_udu(aexp, v) ->
        try_udu

      try_parts = integ_parts(aexp, v) ->
        try_parts

      true ->
        {:integ, aexp, v}
    end
  end

  # Fallthrough
  def integ(c, v) do
    {:integ, c, v}
  end

  def symbinteg(ast) do
    case ast do
      {:integ, _, _} -> true
      {:fcall, _, args} -> Enum.reduce(args, true, fn el, ac -> symbinteg(el) or ac end)
      {{:m, _}, args} -> Enum.reduce(args, false, fn el, ac -> symbinteg(el) or ac end)
      {_, l, r} -> symbinteg(l) and symbinteg(r)
      _ -> false
    end
  end

  @doc """
  Change type operation 'from' to another type 'to' in an ast. Not used for now, try to not perform
  optimization on tuple {:integral} to avoid recursion, but for now I will approach this problem
  on a more elegant way (Pattern matching). Leave this code for future needs
  """
  def mutate(ast, from, to) do
    case ast do
      {^from, _} = {:numb, n} -> {to, n}
      {^from, _, _} = {:unit, a, b} -> {to, mutate(a, from, to), mutate(b, from, to)}
      {^from, _} = {:vari, a} -> {to, a}
      {^from, _, _} = {:fcall, f, args} -> {to, f, Enum.map(args, &mutate(&1, from, to))}
      {^from, _} = {:minus, a} -> {:minus, mutate(a, from, to)}
      {^from, _, _} = {:deriv, f, v} -> {to, mutate(f, from, to), mutate(v, from, to)}
      {^from, _, _} = {:integ, f, v} -> {to, mutate(f, from, to), mutate(v, from, to)}
      {^from, _} = {{:m, :suma}, list} -> {to, Enum.map(list, &mutate(&1, from, to))}
      {^from, _} = {{:m, :mult}, list} -> {to, Enum.map(list, &mutate(&1, from, to))}
      {^from, _, _} = {:elev, a, b} -> {to, mutate(a, from, to), mutate(b, from, to)}
      {:numb, n} -> {:numb, n}
      {:unit, a, b} -> {:unit, mutate(a, from, to), mutate(b, from, to)}
      {:vari, a} -> {:vari, a}
      {:fcall, f, args} -> {:fcall, f, Enum.map(args, &mutate(&1, from, to))}
      {:minus, a} -> {:minus, mutate(a, from, to)}
      {:deriv, f, v} -> {:deriv, mutate(f, from, to), mutate(v, from, to)}
      {:integ, f, v} -> {:integ, mutate(f, from, to), mutate(v, from, to)}
      {{:m, :suma}, list} -> {{:m, :suma}, Enum.map(list, &mutate(&1, from, to))}
      {{:m, :mult}, list} -> {{:m, :mult}, Enum.map(list, &mutate(&1, from, to))}
      {:elev, a, b} -> {:elev, mutate(a, from, to), mutate(b, from, to)}
    end
  end

  def integ_poly(mult = {{:m, :mult}, _}, v = {:vari, x}) do
    case Exun.Pattern.match_ast(Exun.parse_text("a*#{x}^b"), mult, %{}) do
      [] ->
        false

      matchlist ->
        Enum.reduce(matchlist, [], fn {_, map}, listsol ->
          a = Map.fetch!(map, {:vari, "a"})
          var = Map.fetch!(map, {:vari, "#{x}"})
          b = Map.fetch!(map, {:vari, "b"})

          if Exun.Fun.contains(a, v) or Exun.Fun.contains(b, v) or var != {:vari, "#{x}"} do
            listsol
          else
            newexpon = suma(b, @uno)
            [mult(divi(a, newexpon), elev(v, newexpon)) | listsol]
          end
        end)
        |> List.first()
    end
  end

  def integ_udu(ast, {:vari, x}) do
    udu = Exun.parse_text("u*u'#{x}")

    case Exun.Pattern.match_ast(udu, ast) do
      [] ->
        false

      matchlist ->
        Enum.reduce(matchlist, [], fn {_, map}, listsol ->
          u = Map.fetch!(map, {:vari, "u"})
          [divi(elev(u, {:numb, 2}), {:numb, 2}) | listsol]
        end)
        |> List.first()
    end
  end

  def integ_parts(aexp = {{:m, :mult}, _}, {:vari, x}) do
    # try to integrate by parts. We are going to use Pattern.match over the whole expression
    aast = Exun.parse_text("u*v'#{x}")
    vdu = Exun.parse_text("v*u'#{x}")

    solutions =
      Exun.Pattern.match_ast(aast, aexp)
      |> Enum.reject(fn {res, _} -> res != :ok end)
      |> Enum.map(fn {_, map} ->
        # IO.inspect(map, label: "map")

        # look for the other integ, v(x) * u(x)'x, only in the event
        # that a simplification removes the integral we can use it
        otherInteg =
          replace(vdu, map)
          |> mutate(:integ, :sinteg)
          |> Exun.Simpl.mkrec()
          |> mutate(:sinteg, :integ)

        if not symbinteg(otherInteg) do
          uvvdu = Exun.parse_text("u*v-v*u'#{x}")
          {:ok, replace(uvvdu, map) |> Exun.Simpl.mkrec()}
        else
          {:ko, nil}
        end

        # |> IO.inspect(label: "Solution")

        # Check cyclic definitions including {:integ,aexp,v}
        # so the try will not enter an infinite loop.
        # Integrating by parts "2*x" if u=x and v'=2 then u*v-integ(v*u')
        # will be cyclic because v*u' is 2*x, the same original expression
        # First sense of this problem was tryin to integrate polynomials by parts,
        # that was solved previosly
      end)
      |> Enum.reject(fn {r, _} -> r != :ok end)

    case List.first(solutions) do
      nil -> nil
      {:ok, ast} -> ast
    end
  end
end
