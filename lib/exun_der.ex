defmodule Exun.Der do
  import Exun.Fun

  @moduledoc """
  Derivate expressions
  """
  @zero {:numb, 0}
  @uno {:numb, 1}
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
  """
  def deriv(txt, x) when is_binary(txt) and is_binary(x) do
    {ast, _ctx} = Exun.parse(txt)

    deriv(ast, x)
    # |> IO.inspect(label: "reduced")
    |> Exun.UI.tostr()
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
      (bfunc = base()[search_name]) != nil ->
        {ast, _ctx} = Exun.parse(elem(bfunc, 1))
        replace_args_internal(ast, args, {:vari, "x"})

      (cfunc = compounds()[search_name]) != nil ->
        {ast, _ctx} = Exun.parse(cfunc)

        replace_args_internal(ast, args, {:vari, "x"})
        |> der(x)

      true ->
        {:deriv, {:fcall, name, args}, x}
    end

    # |> IO.inspect(label: "der fcall")
  end

  defp der({:minus, a}, x), do: {:minus, der(a, x)}
  defp der({:numb, _}, _x), do: @zero
  defp der({:unit, _uv, _ut}, _x), do: @zero
  defp der({:vari, var}, {:vari, x}), do: if(var == x, do: @uno, else: @zero)

  defp der({:elev, base, expon}, x),
    do:
      mult(
        {:elev, base, expon},
        suma(
          mult(der(expon, x), {:fcall, "ln", [base]}),
          mult(expon, divi(der(base, x), base))
        )
      )

  defp der({:integ, f, x}, x), do: f

  defp der({{:m, :suma}, lst}, x),
    do: {{:m, :suma}, Enum.map(lst, &der(&1, x))}

  defp der({{:m, :mult}, [a]}, x), do: der(a, x)

  defp der({{:m, :mult}, lst}, x) when is_list(lst) do
    prim = List.first(lst)
    segu = {{:m, :mult}, List.delete(lst, prim)}

    suma(
      mult(prim, der(segu, x)),
      mult(der(prim, x), segu)
    )
  end

  defp der(f, x) do
    {:deriv, f, x}
  end
end
