defmodule Exun do
  alias Exun.Tree
  alias Exun.Cyclic

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
         {:ok, utree} <- :exun_yacc.parse(toks) do
      Tree.expand(utree, context, %{})
    end
  end

end
