defmodule Exun do
  alias Exun.Tree
  alias Exun.Cyclic

  @doc ~S"""
  Parse a math expression, without context:
    iex(1)> Exun.parse "x*y^(1+x)"
    {:mult, {:vari, "x"}, {:elev, {:vari, "y"}, {:suma, {:numb, 1}, {:vari, "x"}}}}

  """
  def parse(txt) do
    {t, _} = parse(txt, %{})
    t
  end

  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.
    iex> Exun.parse( "x[m^2]", %{"x"=>"3"})
    {{:unit, {:vari, "x"}, {:elev, {:vari, "m"}, {:numb, 2}}}, %{"x" => {:numb, 3}}}

  returns a tuple {expression, parsed_conext} where
  expression is a tuple that holds math AST and
  parsed_context is a map whith all equalities (definitions) parsed as
  "name" => expression
  """
  def parse(txt, context) do
    case Cyclic.check(context) do
      {:ok, _deps} ->
        tree = parse_text(txt)

        {tree,
         for {func, defi} <- context, into: %{} do
           subtree = parse_text(defi)
           {func, subtree}
         end}

      {:err,msg,_lst} ->
        throw msg
    end
  end

  @doc """
  Parse and evaluate an expression. If ast is true returns de AST tuple,
  if it is false return a human-readable (and parseable) expression
    iex> Exun.eval "x[m^2]+4[cm^2]",%{"x"=>"3"}
    "3.04[m^2]"
  """
  def eval(txt, context) do
    {ast, pcontext} = parse(txt, context)
    ast
    |> Tree.reduce
    |> Tree.replace(pcontext)
    |> Tree.reduce
    |> Tree.tostr
  end

  def eval(txt) do
    eval(txt, %{})
  end

  def parse_text(txt) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      tree
    end
  end
end
