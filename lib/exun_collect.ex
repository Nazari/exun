defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Simpl
  alias Exun.Eq
  alias Exun.Fun

  @doc """
  Main collecting function. Try to simplify tree without chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> Simpl.mkrec()
      |> expand()
      |> Simpl.mkrec()

    if Eq.eq(newtree, tree), do: newtree, else: coll(newtree)
  end

  @doc """
  Expand mult(sum) to try collect more
  """
  def expand({{:m, :suma}, l}), do: {{:m, :suma}, Enum.map(l, &expand(&1))}
  def expand({:deriv, f, v}), do: {:deriv, expand(f), v}
  def expand({:integ, f, v}), do: {:integ, expand(f), v}
  def expand({:fcall, f, args}), do: {:fcall, f, Enum.map(args, &expand(&1))}
  def expand({:unit, n, t}), do: {:unit, expand(n), t}
  def expand({:minus, a}), do: {:minus, expand(a)}
  def expand({:elev, b, e}), do: {:elev, expand(b), expand(e)}

  def expand({{:m, :mult}, l}) do
    tsuma = List.keyfind(l, {:m, :suma}, 0)

    if tsuma != nil do
      # Convert {:m,:mult} to {:m,:suma}
      remain = {{:m, :mult}, List.delete(l, tsuma)}
      {_, lsuma} = tsuma
      {{:m, :suma}, Enum.map(lsuma, &Fun.mult(remain, &1))}
    else
      {{:m, :mult}, Enum.map(l, &expand(&1))}
    end
  end

  def expand(ast), do: ast
end
