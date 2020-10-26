defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Simpl
  alias Exun.Eq

  @doc """
  Main collecting function. Try to simplify tree without chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> Simpl.mkrec()

    if Eq.eq(newtree, tree), do: newtree, else: coll(newtree)
  end
end
