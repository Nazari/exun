defmodule Exun do
  alias Exun.Tree
  alias Exun.Cyclic

  @doc ~S"""
  Parse a math expression, without context:
    iex(1)> Exun.parse "x*y^(1+x)"
    {:mult, {:vari, "x"}, {:elev, {:vari, "y"}, {:suma, {:numb, 1}, {:vari, "x"}}}}

  """
  def parse(txt) do
    {t,_}=parse(txt, %{})
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
        {tree, _deps} = parse_text(txt, context)

        {tree,
         for {func, defi} <- context, into: %{} do
           {subtree, _deps} = parse_text(defi, context)
           {func, subtree }
         end}

      a ->
        a
    end
  end
  @doc """
  Parse and evaluate an expression. If ast is true returns de AST tuple,
  if it is false return a human-readable (and parseable) expression
    iex> Exun.eval "x[m^2]+4[cm^2]",%{"x"=>"3"}
    "3.04[m^2]"
  """
  def eval(txt, context, ast \\ false) do
    {expr, pcont} = parse(txt,context)
    evaluated = eval_repl(expr,pcont)
              |> Tree.reduce()
    if ast, do: evaluated, else: Exun.Tree.tostr(evaluated)
  end

  def eval(txt) do
    eval(txt,%{})
  end

  def eval_repl(expr, pcontext) do
    case expr do
      {:vari, var} ->
        Map.get(pcontext, var, {:vari, var})
      {op, l, r} ->
        {op,
          eval_repl(l, pcontext),
          eval_repl(r, pcontext)
        }
      other ->
        other
    end
  end

  defp parse_text(txt, context) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      Tree.replace(tree, context, %{})
    end
  end
end
