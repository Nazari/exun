defmodule Exun.Der do
  alias Exun.Simpl, as: S
  alias Exun.Fun, as: F

  @zero {:numb, 0, 1}
  @uno {:numb, 1, 1}
  @moduledoc """
  Derivate expressions
  """

  @doc """
  Derive function for variable x, may be an ast or an expresion in text mode
  ```
  deriv("sin(x)","x")
  -> cos(x)*x'x
  ```
   Or an AST, for internal use of library
  ```
  deriv({:vari,"x"},"x")
  -> {:numb,1}
  ```
  """
  def deriv(txt, x) when is_binary(txt) and is_binary(x) do
    ast = Exun.new(txt).ast

    deriv(ast, x)
    # |> IO.inspect(label: "reduced")
    |> Exun.UI.tostr()
  end

  def deriv({:deriv, f, v}) do
    der(f, v)
  end

  defp der(ast, x) do
    case ast do
      {:numb, _, _} ->
        @zero

      {:unit, _uv, _ut} ->
        @zero

      ^x ->
        @uno

      {:vari, _} ->
        @zero

      {:deriv, f2, v2} ->
        der(der(f2, v2), x)

      {:fcall, name, args} ->
        search_name = "#{name}(F)"

        cond do
          (bfunc = F.base()[search_name]) != nil ->
            ast = Exun.new(elem(bfunc, 1)).ast
            mapdef = %{{:vari, "F"} => args |> List.first(), {:vari, "x"} => {:vari, "x"}}
            Exun.replace(ast, mapdef)

          (cfunc = F.compounds()[search_name]) != nil ->
            ast = Exun.new(cfunc).ast
            mapdef = %{{:vari, "F"} => args |> List.first(), {:vari, "x"} => {:vari, "x"}}

            Exun.replace(ast, mapdef)
            |> der(x)

          true ->
            {:deriv, {:fcall, name, args}, x}
        end

      {:minus, a} ->
        {:minus, der(a,x)}

      {:elev, base, expon} ->
        S.mult(
          {:elev, base, expon},
          S.suma(
            S.mult(der(expon, x), {:fcall, "ln", [base]}),
            S.mult(expon, S.divi(der(base, x), base))
          )
        )

      {:integ, f, ^x} ->
        f

      {{:m, :suma}, lst} ->
        {{:m, :suma}, Enum.map(lst, &der(&1, x))}

      {{:m, :mult}, lst} ->
        case length(lst) do
          0 ->
            @zero

          1 ->
            der(lst |> List.first(), x)

          2 ->
            [prim, segu] = lst
            derprod(prim, segu, x)

          _ ->
            prim = List.first(lst)
            segu = {{:m, :mult}, List.delete(lst, prim)}
            derprod(prim, segu, x)
        end

      {tt = {:vector, _}, list} ->
        {tt, list |> Enum.map(&deriv({:deriv, &1, x}))}

      {tt = {:raw, _, _}, list, mr, mc} ->
        {tt, list |> Enum.map(&deriv({:deriv, &1, x})), mr, mc}

      _other ->
        @zero
    end
  end

  defp derprod(prim, segu, x) do
    S.suma(
      S.mult(prim, der(segu, x)),
      S.mult(der(prim, x), segu)
    )
  end
end
