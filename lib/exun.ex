defmodule Exun do
  alias Exun.Cyclic
  alias Exun.Simpl
  alias Exun.Collect
  alias Exun.Eq
  alias Exun.UI

  @moduledoc """
  Symbolic Math for Elixir, with Units support
  """

  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.
  ```
    iex> Exun.parse( "x[m^2]", %{"x"=>"3"})
    {{:unit, {:vari, "x"}, {:elev, {:vari, "m"}, {:numb, 2, 1}}}, %{{:vari, "x"} => {:numb, 3, 1}}}

  ```
  returns a tuple {expression, parsed_context} where
  expression is a tuple that holds math AST and
  parsed_context is a map whith all equalities (definitions) parsed as
  "name" => expression
  See parse_txt for an explanation
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
    ast_eval(ast, pctx)
  end

  @doc """
  Eval an ast (not text expression)
  """
  def ast_eval(ast, pctx \\ %{}) do
    case ast do
      {:error, {line, _app, list}} ->
        throw("Error line:#{line} #{list}")

      _ ->
        # First Collect context
        pctx =
          for {k, v} <- pctx, into: %{} do
            {k, Collect.coll(v)}
          end

        ast
        # |> IO.inspect(label: "eval01,AST")
        |> replace(pctx)
        # |> IO.inspect(label: "eval02,Replaced")
        |> Simpl.mkrec()

        # |> IO.inspect(label: "eval03,mkrec")
    end
  end

  @doc """
  Parses an expression in text format. Special symbols used:
  - An Unit: <number>[UnitExpression] : 1[m/s]
  - A multiplication, division, suma, rest and parenthesis works in the usual way: 2**x-3**(y-2)
  - Function call: fname(args): sin(x)
  - A derivate: <expression>'<var> : sin(x)'x. Symbol (') has the greatest priority, for example
  y^x'x is equals to y^(x'x). Use () to be clear: (y^x)'x
  - An integral: $<expression>,<var>: $tan(x),x
  - You can sum units: 1[m]+1[cm] if are of the same magnitude. For example 1[m]+1[s] will
  throw an exception. You can multiple/divide any two unis.
  """
  @spec parse_text(binary) :: any
  def parse_text(txt) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      tree
      |> walkn()
    end
  end

  @doc """
  Replace definitions in ParsedContext (pc) in tree
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
      {:unit, uv, ut} ->
        {:unit, replace(uv, pc), replace(ut, pc)}

      {:vari, var} ->
        Map.get(pc, {:vari, var}, {:vari, var})

      {:minus, a} ->
        {:minus, repl(a, pc)}

      {:numb, _, _} ->
        tree

      {{:vector, s}, l} ->
        {{:vector, s}, Enum.map(l, &replace(&1, pc))}

      {{t, rs, cs}, list, mr, mc} ->
        {{t, rs, cs}, list |> Enum.map(&replace(&1, pc)), mr, mc}

      {{:m, op}, lst} ->
        {{:m, op}, Enum.map(lst, &replace(&1, pc))}

      {:elev, l, r} ->
        {:elev, replace(l, pc), replace(r, pc)}

      {:integ, f, v = {:vari, _}} ->
        {:integ, replace(f, pc), replace(v, pc)}

      {:deriv, f, v = {:vari, _}} ->
        {:deriv, replace(f, pc), replace(v, pc)}

      {:fcall, name, args} ->
        args = Enum.map(args, &repl(&1, pc))
        arity = length(args)

        user_function =
          Map.keys(pc)
          # |> IO.inspect(label: "user_function1")
          |> Enum.filter(fn el -> elem(el, 0) == :fcall end)
          # |> IO.inspect(label: "user_function2")
          |> Enum.filter(fn el -> elem(el, 1) == name and length(elem(el, 2)) == arity end)

        # |> IO.inspect(label: "#{Exun.UI.tostr({:fcall, name, args})}")

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

            replace(ast, nv)

          true ->
            {:fcall, name, args}
        end

      unknown ->
        throw("Replace unknown #{Exun.UI.tostr(unknown)}")
    end

    # |> IO.inspect(label: label)
  end

  @doc """
  Changes all numbers to Decimal struct
  """
  def walkn(ast) do
    case ast do
      {:numb, n} ->
        Exun.Math.mknum(n * 1000, 1000)

      {:fcall, name, args} ->
        {:fcall, name, Enum.map(args, &walkn/1)}

      {{:m, op}, list} ->
        {{:m, op}, Enum.map(list, &walkn/1)}

      {:vari, x} ->
        {:vari, x}

      {:minus, a} ->
        {:minus, walkn(a)}

      {{:vector, n}, l} ->
        {{:vector, n}, Enum.map(l, &walkn/1)}

      {{:raw, a, b}, list, [], []} ->
        {{:raw, a, b}, Enum.map(list, &walkn/1), [], []}

      {op, n, t} ->
        {op, walkn(n), walkn(t)}
    end
  end
end
