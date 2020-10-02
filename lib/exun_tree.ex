defmodule Exun.Tree do

  def expand(tree, context) do
    expand(tree, context, %{})
  end

  def expand(tree, context, deps) do
    newtree = single_expand(tree, context)

    if tree != newtree do
      expand(newtree, context, deps)
    else
      {tree, deps}
    end
  end

  def single_expand({op, left, right}, context) do
    {op, single_expand(left, context), single_expand(right, context)}
  end

  def single_expand({:vari, var}, context) do
    in_context = Map.get(context, var)

    if in_context != nil do
      with {:ok, toks, _} <- :exun_lex.string(in_context |> to_string() |> String.to_charlist()),
           {:ok, tree} <- :exun_yacc.parse(toks) do
        tree
      end
    else
      {:vari, var}
    end
  end

  def single_expand(a, _context) do
    a
  end
end
