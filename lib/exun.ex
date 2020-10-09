defmodule Exun do
  alias Exun.Cyclic
  alias Exun.Collect

  @defop %{
    :elev => {100, "^"},
    :mult => {90, "*"},
    :divi => {90, "/"},
    :suma => {50, "+"},
    :rest => {50, "-"},
    :numb => {200, nil},
    :unit => {200, nil},
    :vari => {200, nil}
  }
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

      {:err, msg, _lst} ->
        throw(msg)
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

    case ast do
      {:error, {line, _app, list}} ->
        throw("Error line:#{line} #{list}")

      _ ->
        ast
        #|> IO.inspect(label: "eval01,AST:")
        |> Collect.make()
        #|> IO.inspect(label: "eval02,make:")
        |> replace(pcontext)
        #|> IO.inspect(label: "eval03,replace:")
        |> Collect.make()
        #|> IO.inspect(label: "eval04,make:")
        |> tostr()
    end
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

  @doc ~S"""
  Translate tree to human readable math expression:
    iex(1)> {tree, deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
    {{:divi,
    {:mult, {:numb, 4}, {:elev, {:vari, "x"}, {:suma, {:vari, "y"}, {:numb, 1}}}},
    {:vari, "z"}}, %{"z" => {:suma, {:vari, "y"}, {:numb, 1}}}}

  """
  def tostr({:vari, var}) do
    var
  end

  def tostr({:unit, n, tree}) do
    tostr(n) <> "[" <> tostr(tree) <> "]"
  end

  def tostr({:numb, n}) do
    if n == floor(n), do: to_string(floor(n)), else: to_string(n)
  end

  def tostr({op, l, r}) do
    {hpri, hstr} = @defop[op]
    {lpri, _} = @defop[l |> elem(0)]
    {rpri, _} = @defop[r |> elem(0)]

    ltxt = tostr(l)
    rtxt = tostr(r)

    cond do
      hpri > lpri and hpri > rpri ->
        "(" <> ltxt <> ")" <> hstr <> "(" <> rtxt <> ")"

      hpri > lpri ->
        "(" <> ltxt <> ")" <> hstr <> rtxt

      hpri > rpri ->
        ltxt <> hstr <> "(" <> rtxt <> ")"

      true ->
        ltxt <> hstr <> rtxt
    end
  end

  @doc """
  Expand definitions in context into
  main tree expression until no more
  expansion is posssible
  """
  def replace(tree, pc) do
    newtree = repl(tree, pc)

    if not eq(tree, newtree) do
      replace(newtree, pc)
    else
      newtree
    end
  end

  def repl({:vari, var}, pc) do
    Map.get(pc, var, {:vari, var})
  end

  def repl({op, l, r}, pc) do
    {op, replace(l, pc), replace(r, pc)}
  end

  def repl(other, _pc) do
    other
  end

  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    Collect.norm(t1) == Collect.norm(t2)
  end
end
