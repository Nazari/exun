defmodule Exun.Integral do
  alias Exun.Fun, as: F
  alias Exun.Simpl, as: S
  alias Exun.Pattern, as: P
  import Exun

  @moduledoc """
  Try to integrate function
  """
  @zero {:numb, 0, 1}
  @uno {:numb, 1, 1}
  @muno {:numb, -1, 1}
  @dos {:numb, 2, 1}

  @doc """
  Integrate function for var v. Expect an AST and return an AST.
  """
  def integ({:integ, ast, var}) do
    case {ast, var} do
      {@zero, _} ->
        @uno

      {{:minus, @zero}, _} ->
        @muno

      {{:minus, a}, v} ->
        {:minus, integ({:integ, a, v})}

      {n = {:numb, _, _}, v} ->
        S.mult(n, v)

      {v = {:vari, _}, v} ->
        S.mult({:numb, 1, 2}, {:elev, v, @dos})

      {{:deriv, f, x}, x} ->
        f

      {{:vari, a}, v} ->
        S.mult({:vari, a}, v)

      {{:elev, {:vari, v}, expon = {:numb, _, _}}, {:vari, v}} ->
        newexp = S.suma(expon, @uno)
        S.mult({:elev, {:vari, v}, newexp}, S.chpow(newexp))

      {{:fcall, "sin", [v = {:vari, x}]}, v} ->
        snew("-cos(x)", x)

      {{:fcall, "cos", [v = {:vari, x}]}, v} ->
        snew("sin(x)", x)

      {{:fcall, "tan", [v = {:vari, x}]}, v} ->
        snew("-ln(cos(x))", x)

      {{:fcall, "asin", [v = {:vari, x}]}, v} ->
        snew("x*asin(x)+(1-x^2)^0.5", x)

      {{:fcall, "acos", [v = {:vari, x}]}, v} ->
        snew("x*acos(x)-(1-x^2)^0.5", x)

      {{:fcall, "atan", [v = {:vari, x}]}, v} ->
        snew("x*atan(x)-ln(x^2+1)/2", x)

      {{:fcall, "sinh", [v = {:vari, _x}]}, v} ->
        {:fcall, "cosh", [v]}

      {{:fcall, "cosh", [v = {:vari, _x}]}, v} ->
        {:fcall, "sinh", [v]}

      {{:fcall, "tanh", [v = {:vari, x}]}, v} ->
        snew("ln(cosh(x))", x)

      {{:fcall, "asinh", [v = {:vari, x}]}, v} ->
        snew("x*asinh(x)-(x^2+1)^0.5", x)

      {{:fcall, "acosh", [v = {:vari, x}]}, v} ->
        snew("x*acosh(x)-(1+x)*((x-1)/(x+1))^0.5", x)

      {{:fcall, "atanh", [v = {:vari, x}]}, v} ->
        snew("(ln(1+x)+2*x*atanh(x)+ln(1-x))/2", x)

      {{:fcall, name, args}, v} ->
        case F.base()[name <> "(F)"] do
          {_, _, val, _} when val != nil ->
            ast = Exun.new(val).ast
            mapdef = %{{:vari, "F"} => args |> List.first(), {:vari, "x"} => {:vari, "x"}}
            Exun.replace(ast, mapdef)

          _ ->
            {:integ, {:fcall, name, args}, v}
        end

      {{{:m, :suma}, lst}, v} ->
        {{:m, :suma}, Enum.map(lst, &integ({:integ, &1, v}))}

      {aexp = {{:m, :mult}, _}, v} ->
        cond do
          try_poly = integ_poly(aexp, v) ->
            try_poly

          try_udu = integ_udu(aexp, v) ->
            try_udu

          # try_parts = integ_parts(aexp, v) ->
          # try_parts

          true ->
            {:integ, aexp, v}
        end

      {c, v} ->
        {:integ, c, v}
    end
  end

  defp snew(str, x) do
    (str |> String.replace("x", x) |> new()).ast
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
      {^from, _, _} = {:numb, n, d} -> {to, n, d}
      {^from, _, _} = {:unit, a, b} -> {to, mutate(a, from, to), mutate(b, from, to)}
      {^from, _} = {:vari, a} -> {to, a}
      {^from, _, _} = {:fcall, f, args} -> {to, f, Enum.map(args, &mutate(&1, from, to))}
      {^from, _} = {:minus, a} -> {:minus, mutate(a, from, to)}
      {^from, _, _} = {:deriv, f, v} -> {to, mutate(f, from, to), mutate(v, from, to)}
      {^from, _, _} = {:integ, f, v} -> {to, mutate(f, from, to), mutate(v, from, to)}
      {^from, _} = {{:m, :suma}, list} -> {to, Enum.map(list, &mutate(&1, from, to))}
      {^from, _} = {{:m, :mult}, list} -> {to, Enum.map(list, &mutate(&1, from, to))}
      {^from, _, _} = {:elev, a, b} -> {to, mutate(a, from, to), mutate(b, from, to)}
      {:numb, n, d} -> {:numb, n, d}
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
    case P.match_ast(new("a*#{x}^b").ast, mult, %{}) do
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
            newexpon = S.suma(b, @uno)
            [S.mult(S.divi(a, newexpon), S.elev(v, newexpon)) | listsol]
          end
        end)
        |> List.first()
    end
  end

  def integ_udu(ast, {:vari, x}) do
    udu = Exun.new("u*u'#{x}").ast

    case Exun.Pattern.match_ast(udu, ast) do
      [] ->
        false

      matchlist ->
        Enum.reduce(matchlist, [], fn {_, map}, listsol ->
          u = Map.fetch!(map, {:vari, "u"})
          [S.divi(S.elev(u, @dos), @dos) | listsol]
        end)
        |> List.first()
    end
  end

  def integ_parts(aexp = {{:m, :mult}, _}, {:vari, x}) do
    # try to integrate by parts. We are going to use Pattern.match over the whole expression
    aast = Exun.new("u*v'#{x}").ast
    vdu = Exun.new("v*u'#{x}").ast

    solutions =
      Exun.Pattern.match_ast(aast, aexp)
      |> Enum.reject(fn {res, _} -> res != :ok end)
      |> Enum.map(fn {_, map} ->
        # IO.inspect(map, label: "map")

        # look for the other integ, v(x) * u(x)'x, only in the event
        # that a simplification removes the integral we can use it
        otherInteg =
          replace(vdu, map)
          # |> mutate(:integ, :sinteg)
          |> Exun.Simpl.mkrec()

        # |> mutate(:sinteg, :integ)

        if not symbinteg(otherInteg) do
          uvvdu = Exun.new("u*v-v*u'#{x}").ast
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
