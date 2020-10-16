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
    :equal => {20, "="},
    :numb => {200, nil},
    :unit => {200, nil},
    :vari => {200, nil},
    :fcall => {200, nil},
    :deriv => {110, "'"}
  }
  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.
  ```
    iex> Exun.parse( "x[m^2]", %{"x"=>"3"})
    {{:unit, {:vari, "x"}, {:elev, {:vari, "m"}, {:numb, 2}}}, %{{:vari, "x"} => {:numb, 3}}}

  ```
  returns a tuple {expression, parsed_conext} where
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
    |> tostr()
  end

  @doc """
  Same as eval but returns AST
  """
  def eval_ast(txt, context \\ %{}) do
    {ast, pctx} = parse(txt, context)
    # |> IO.inspect(label: "ast and pctx")

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

  @doc ~S"""
  Translate tree to human readable math expression:
  ```
    iex(1)> {_tree, _deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
    {{:divi,
    {:mult, {:numb, 4}, {:elev, {:vari, "x"}, {:suma, {:vari, "y"}, {:numb, 1}}}},
    {:vari, "z"}}, %{{:vari, "z"} => {:suma, {:vari, "y"}, {:numb, 1}}}}

  ```
  """
  def tostr(tree) do
    tree
    # |> IO.inspect(label: "tostr1,orig")
    |> Collect.denorm()
    # |> IO.inspect(label: "tostr2,denorm")
    |> its()
    |> aesthetic()
  end

  defp aesthetic(str) do
    newstr = aest(str)
    if str == newstr, do: newstr, else: aest(newstr)
  end

  defp aest(str) do
    %{"+-" => "-", "-+" => "-", "--" => "+", "++" => "+"}
    |> Enum.reduce(str, fn {k, v}, str -> String.replace(str, k, v) end)
  end

  defp its({:mult, {:numb, -1}, a}) do
    "-" <> its(a)
  end

  defp its({:mult, a, {:numb, -1}}) do
    "-" <> its(a)
  end

  defp its({:mult, {:divi, {:numb, 1}, a}, b}) do
    its({:divi, b, a})
  end

  defp its({:mult, a, {:elev, b, {:numb, n}}}) when n < 0 do
    its({:divi, a, {:elev, b, {:numb, -n}}})
  end

  defp its({:mult, {:elev, b, {:numb, n}}, a}) when n < 0 do
    its({:divi, a, {:elev, b, {:numb, -n}}})
  end

  defp its({:mult, b, {:divi, {:numb, 1}, a}}) do
    its({:divi, b, a})
  end

  defp its({:vari, var}) do
    var
  end

  defp its({:elev, a, {:numb, 1}}) do
    its(a)
  end

  defp its({:elev, a, {:numb, -1}}) do
    its({:divi, {:numb, 1}, a})
  end

  defp its({:fcall, name, args}) when is_list(args) do
    name <>
      "(" <>
      Enum.reduce(args, "", fn el, ac ->
        case ac do
          "" -> its(el)
          _ -> ac <> ", " <> its(el)
        end
      end) <> ")"
  end

  defp its({:unit, n, tree}) do
    its(n) <> "[" <> its(Collect.coll(tree)) <> "]"
  end

  defp its({:numb, n}) do
    if n == floor(n), do: to_string(floor(n)), else: to_string(n)
  end

  defp its({:deriv, a, {:vari, x}}) do
    its(a) <> "'" <> x
  end

  defp its({op, l, r}) do
    # IO.inspect([op,l,r])
    {hpri, hstr} = @defop[op]
    {lpri, _} = @defop[l |> elem(0)]
    {rpri, _} = @defop[r |> elem(0)]

    ltxt = its(l)
    rtxt = its(r)
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
  Replace definitions in context into
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

  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    Collect.norm(t1) == Collect.norm(t2)
  end
end
