defmodule Exun do
  alias Exun.Tree
  alias Exun.Cyclic
  @doc ~S"""
  Parse a math expression, without context:
    iex(1)> Exun.parse "x*y^(1+x)"
    {{:mult, {:vari, "x"}, {:elev, {:vari, "y"}, {:suma, {:numb, 1}, {:vari, "x"}}}},
    %{}}

  """
  def parse(txt) do
    parse(txt, %{})
  end

  def parse(txt, context) do
    case Cyclic.check(context) do
      {:ok, _deps} ->
        {tree, _deps} = parse_text(txt, context)

        {tree |> Tree.collect,
         for {func, defi} <- context, into: %{} do
           {subtree, _deps} = parse_text(defi, context)
           {func, subtree |> Tree.collect}
         end}

      a ->
        a
    end
  end

  def parse_text(txt, context) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      Tree.expand(tree, context, %{})
    end
  end

end
