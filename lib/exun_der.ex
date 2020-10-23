defmodule Exun.Der do
  import Exun.Fun
  import Exun.MProc

  @moduledoc """
  Derivate expressions
  """
  @zero {:numb, 0}
  @uno {:numb, 1}
  @doc """
  Derive function txt for variable x, return string
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
  end

  defp der({:numb, _}, _x),
    do: @zero

  defp der({:unit, _uv, _ut}, _x),
    do: @zero

  defp der({:vari, var}, {:vari, x}),
    do: if(var == x, do: @uno, else: @zero)

  defp der({:suma, a, b}, x),
    do: parallel({:suma, der(a, x), der(b, x)})

  defp der({:rest, a, b}, x),
    do: parallel({:rest, der(a, x), der(b, x)})

  defp der({:mult, a, b}, x),
    do: parallel({:suma, {:mult, der(a, x), b}, {:mult, a, der(b, x)}})

  defp der({:divi, a, b}, x),
    do:
      parallel(
        {:divi, {:rest, {:mult, b, der(a, x)}, {:mult, der(b, x), a}}, {:elev, b, {:numb, 2}}}
      )

  defp der(y = {:elev, f, g}, x),
    do:
      {:mult, y,
       parallel(
         {:suma, {:mult, der(g, x), {:fcall, "ln", [f]}}, {:mult, g, {:divi, der(f, x), f}}}
       )}

  defp der({:integ, f, x}, x), do: f

  defp der({{:m, :suma}, lst}, x), do: {{:m, :suma}, Enum.map(lst, &der(&1, x))}

  defp der(a = {{:m, :mult}, _lst}, x) do
    der(Exun.Eq.denorm(a), x)
  end

  defp der(f, x) do
    {:deriv, f, x}
  end
end
