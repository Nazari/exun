defmodule Exun do
  alias Exun.Cyclic
  alias Exun.Collect
  alias Exun.Eq
  alias Exun.UI

  def debug() do
    eval "$x/x,x"
  end

  @moduledoc """
  Symbolic Math for Elixir, with Units support
  """

  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.
  ```
    iex> Exun.parse( "x[m^2]", %{"x"=>"3"})
    {{:unit, {:vari, "x"}, {:elev, {:vari, "m"}, {:numb, 2}}}, %{{:vari, "x"} => {:numb, 3}}}

  ```
  returns a tuple {expression, parsed_context} where
  expression is a tuple that holds math AST and
  parsed_context is a map whith all equalities (definitions) parsed as
  "name" => expression
  """
  def parse(txt, context \\ %{}) do
    case Cyclic.check(context) do
      {:ok, _deps} ->
        tree = parse_text(txt)

        {tree,
         for {func, defi} <- context, into: %{} do
           {parse_text(func), parse_text(defi)}
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
  def eval(txt, context \\ %{}) do
    eval_ast(txt, context)
    |> UI.tostr()
  end

  @doc """
  Same as eval but returns AST
  """
  def eval_ast(txt, context \\ %{}) do
    {ast, pctx} = parse(txt, context)
    # |> IO.inspect(label: "ast and pctx")
    ast_eval(ast,pctx)
  end

  def ast_eval(ast, pctx \\ %{}) do
    case ast do
      {:error, {line, _app, list}} ->
        throw("Error line:#{line} #{list}")

      _ ->
        # First Collect context
        pctx = for {k,v} <- pctx, into: %{} do
          {k, Collect.coll(v)}
        end

        ast
        # |> IO.inspect(label: "eval01,AST")
        |> replace(pctx)
        # |> IO.inspect(label: "eval02,Replaced")
        |> Collect.coll()
    end
  end

  def parse_text(txt) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      tree
    end
  end


  @doc """
  Replace definitions in context into
  main tree expression until no more
  expansion is posssible
  """
  def replace(tree, pc) do
    newtree = repl(tree, pc)

    if not Eq.eq(tree, newtree) do
      replace(newtree, pc)
    else
      newtree
    end
  end

  defp repl(tree, pc) do
    case tree do
      {:vari, var} ->
        Map.get(pc, {:vari, var}, {:vari, var})

      {:fcall, name, args} ->
        args = Enum.map(args, &repl(&1, pc))
        arity = length(args)

        user_function =
          Map.keys(pc)
          # |> IO.inspect(label: "user_function1")
          |> Enum.filter(fn el -> elem(el, 0) == :fcall end)
          # |> IO.inspect(label: "user_function2")
          |> Enum.filter(fn el -> elem(el, 1) == name and length(elem(el, 2)) == arity end)

        # |> IO.inspect(label: "user_function3")

        cond do
          length(user_function) > 1 ->
            {_, dupe_name, _} = user_function |> List.first()
            throw("Multiple definition for function #{dupe_name}")

          length(user_function) == 1 ->
            key = {:fcall, _, args_names} = user_function |> List.first()
            ast = pc[key]

            nv =
              List.zip([args_names, args])
              |> Enum.reduce(%{}, fn {n, v}, ac ->
                Map.put(ac, n, v)
              end)

            replace_args(ast, nv)

          true ->
            {:fcall, name, args}
        end

      {op, l, r} ->
        {op, replace(l, pc), replace(r, pc)}

      _ ->
        tree
    end
  end

  defp replace_args(ast, nv) do
    case ast do
      {:vari, v} ->
        Map.get(nv, {:vari, v}, {:vari, v})

      {:unit, un, ut} ->
        {:unit, replace_args(un, nv), replace_args(ut, nv)}

      {:fcall, subname, subargs} ->
        {:fcall, subname, subargs |> Enum.map(&replace_args(&1, nv))}

      {op, l, r} ->
        {op, replace_args(l, nv), replace_args(r, nv)}

      other ->
        other
    end
  end


end
