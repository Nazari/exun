defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  import Exun.Simpl
  import Exun.Eq
  import Exun.Fun

  @doc """
  Main collecting function. Try to simplify tree withou chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> mkrec()
      |> expand_rec()
      |> mkrec()

    if eq(newtree, tree), do: newtree, else: coll(newtree)
  end

  @doc """
  Expand mult(sum) to try collect more
  """
  def expand_rec(ast) do
    newast = expand(ast)

    if ast == newast do
      newast
    else
      expand_rec(newast)
    end
  end

  defp expand({{:m, :suma}, l}), do: {{:m, :suma}, Enum.map(l, &expand(&1))}
  defp expand({:deriv, f, v}), do: {:deriv, expand(f), v}
  defp expand({:integ, f, v}), do: {:integ, expand(f), v}
  defp expand({:fcall, f, args}), do: {:fcall, f, Enum.map(args, &expand(&1))}
  defp expand({:unit, n, t}), do: {:unit, expand(n), t}
  defp expand({:minus, a}), do: {:minus, expand(a)}
  defp expand({:elev,a,{:numb, n}}) when n>0, do: {{:m,:mult},List.duplicate(a,n)}
  defp expand({:elev, b, e}), do: {:elev, expand(b), expand(e)}

  defp expand({{:m, :mult}, l}) do
    tsuma = List.keyfind(l, {:m, :suma}, 0)

    if tsuma != nil do
      {_, lsuma} = tsuma
      # Convert {:m,:mult} to {:m,:suma}
      remain = {{:m, :mult}, List.delete(l, tsuma)}
      {{:m, :suma}, Enum.map(lsuma, &mult(remain, &1))}
    else
      {{:m, :mult}, Enum.map(l, &expand(&1))}
    end
  end

  defp expand(ast), do: ast
end
