defmodule Exun do
  alias Exun.Cyclic
  alias Exun.Collect

  @moduledoc """
  Symbolic Math for Elixir, with Units support
  """
  @defop %{
    :elev => {100, "^"},
    :mult => {90, "*"},
    :divi => {90, "/"},
    :suma => {50, "+"},
    :rest => {50, "-"},
    :numb => {200, nil},
    :unit => {200, nil},
    :vari => {200, nil},
    :fcall => {200, nil},
    :deriv => {110, "'"}
  }
  @doc ~S"""
  Parse a math expression, without context:
  ```
    iex(1)> Exun.parse "x*y^(1+x)"
    {:mult, {:vari, "x"}, {:elev, {:vari, "y"}, {:suma, {:numb, 1}, {:vari, "x"}}}}

  ```
  """
  def parse(txt) do
    {t, _} = parse(txt, %{})
    t
  end

  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.
  ```
    iex> Exun.parse( "x[m^2]", %{"x"=>"3"})
    {{:unit, {:vari, "x"}, {:elev, {:vari, "m"}, {:numb, 2}}}, %{"x" => {:numb, 3}}}

  ```
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

      {:err, msg} ->
        throw(msg)

      {:err, msg, _lst} ->
        throw(msg)
    end
  end

  @doc """
  Parse and evaluate an expression. If ast is true returns de AST tuple,
  if it is false return a human-readable (and parseable) expression.
  ```
    iex> Exun.eval "x[m^2]+4[cm^2]",%{"x"=>"3"}
    "3.0004[m^2]"

  ```
  """
  def eval(txt, context) do
    eval_ast(txt, context)
    |> tostr()
  end

  @doc """
  Same as eval but returns AST
  """
  def eval_ast(txt, context) do
    {ast, pcontext} = parse(txt, context)

    case ast do
      {:error, {line, _app, list}} ->
        throw("Error line:#{line} #{list}")

      _ ->
        ast
        # |> IO.inspect(label: "eval01,AST")
        |> replace(pcontext)
        # |> IO.inspect(label: "eval02,Replaced")
        |> Collect.coll()
    end
  end

  @doc """
  Same as eval but with empty context
  """
  def eval(txt) do
    eval(txt, %{})
  end

  @doc """
  Same as eval_ast but with empty context
  """

  def eval_ast(txt) do
    eval_ast(txt, %{})
  end

  def parse_text(txt) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      tree
    end
  end

  @doc ~S"""
  Translate tree to human readable math expression:
  ```
    iex(1)> {_tree, _deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
    {{:divi,
    {:mult, {:numb, 4}, {:elev, {:vari, "x"}, {:suma, {:vari, "y"}, {:numb, 1}}}},
    {:vari, "z"}}, %{"z" => {:suma, {:vari, "y"}, {:numb, 1}}}}

  ```
  """
  def tostr(tree) do
    tree
    # |> IO.inspect(label: "tostr1,orig")
    |> Collect.denorm()
    # |> IO.inspect(label: "tostr2,denorm")
    |> innertostr()
  end

  defp innertostr({:mult, {:numb, -1}, a}) do
    "-" <> innertostr(a)
  end

  defp innertostr({:mult, a, {:numb, -1}}) do
    "-" <> innertostr(a)
  end

  defp innertostr({:mult, {:divi, {:numb, 1}, a}, b}) do
    innertostr({:divi, b, a})
  end

  defp innertostr({:mult,a,{:elev,b,{:numb, n}}}) when n<0 do
    innertostr({:divi, a, {:elev,b,{:numb, -n}}})
  end

  defp innertostr({:mult,{:elev,b,{:numb, n}},a}) when n<0 do
    innertostr({:divi, a, {:elev,b,{:numb, -n}}})
  end

  defp innertostr({:mult, b, {:divi, {:numb, 1}, a}}) do
    innertostr({:divi, b, a})
  end

  defp innertostr({:vari, var}) do
    var
  end

  defp innertostr({:fcall, name, args}) when is_list(args) do
    name <>
      "(" <>
      Enum.reduce(args, "", fn el, ac ->
        case ac do
          "" -> innertostr(el)
          _ -> ac <> ", " <> innertostr(el)
        end
      end) <> ")"
  end

  defp innertostr({:unit, n, tree}) do
    innertostr(n) <> "[" <> innertostr(tree) <> "]"
  end

  defp innertostr({:numb, n}) do
    if n == floor(n), do: to_string(floor(n)), else: to_string(n)
  end

  defp innertostr({op, l, r}) do
    # IO.inspect([op,l,r])
    {hpri, hstr} = @defop[op]
    {lpri, _} = @defop[l |> elem(0)]
    {rpri, _} = @defop[r |> elem(0)]

    ltxt = innertostr(l)
    rtxt = innertostr(r)
    conctostr(hpri, hstr, lpri, ltxt, rpri, rtxt)
  end

  defp conctostr(hpri, hstr, lpri, ltxt, rpri, rtxt) do
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

  defp repl({:vari, var}, pc) do
    Map.get(pc, var, {:vari, var})
  end

  defp repl({op, l, r}, pc) do
    {op, replace(l, pc), replace(r, pc)}
  end

  defp repl(lst, pc) when is_list(lst) do
    Enum.map(lst, fn el ->
      repl(el, pc)
    end)
  end

  defp repl(other, _pc) do
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
