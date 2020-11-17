defmodule Exun do
  defstruct ast: nil, pc: %{}

  alias Exun.Simpl, as: S
  alias Exun.Collect, as: C
  alias Exun.Eq, as: E

  @moduledoc """
  Symbolic Math for Elixir, with Units support
  """
  @doc ~S"""
  Parse a math expression, not a 'equality', with context definitions
  For example, express 'x' squared meters, and then define x to be 3.

  Exun.new("x[m^2]", %{"x"=>"3"})
  "3[m^2]"

  Exun.new("x[m^2]", %{"x"=>"3"}).ast
  {:unit, {:numb, 3, 1}, {:elev, {:vari, "m"}, {:numb, 2, 1}}}

  Exun.new("x[m^2]", %{"x"=>"3"}).pc
  %{{:vari, "x"} => {:numb, 3, 1}}

  Exun.UI.tostr(Exun.new( "x[m^2]", %{"x"=>"3"}))
  "3[m^2]"

    returns a tuple {expression, parsed_context} where
  expression is a tuple that holds math AST and
  parsed_context is a map whith all equalities (definitions) parsed as
  "name" => expression

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
  def new(txt, context \\ %{}) do
    case check_cyclic(context) do
      {:ok, _deps} ->
        orig_ast = innerpt(txt)

        parsed_context =
          for {func, defi} <- context, into: %{} do
            {innerpt(func), innerpt(defi)}
          end

        %Exun{
          ast: replace(orig_ast, parsed_context),
          pc: parsed_context
        }

      {:err, msg} ->
        throw(msg)

      {:err, msg, _lst} ->
        throw(msg)
    end
  end

  defp innerpt(txt) do
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, tree} <- :exun_yacc.parse(toks) do
      tree
      |> walkn()
    end
  end

  @doc """
  Eval an ast, returns an ast(not text expression)
  """
  def eval(%Exun{ast: tree}, pctx) do
    eval(%Exun{ast: tree, pc: pctx})
  end

  def eval(tree, pctx) do
    eval(%Exun{ast: tree, pc: pctx})
  end

  def eval(%Exun{ast: tree, pc: pctx}) do
    case tree do
      {:error, {line, _app, list}} ->
        throw("Error line:#{line} #{list}")

      _ ->
        # First Collect context
        pctx =
          for {k, v} <- pctx, into: %{} do
            {k, C.coll(v)}
          end

        %Exun{
          ast:
            tree
            |> replace(pctx)
            |> S.mkrec(),
          pc: pctx
        }
    end
  end

  def eval2str(txt, ctx \\ %{}) do
    exp = new(txt, ctx)
    evaluated = eval(exp)
    Exun.UI.tostr(evaluated.ast)
  end

  @doc """
  Replace definitions in ParsedContext (pc) in tree
  """
  def replace(ast, pc) do
    newast = repl(ast, pc)

    if not E.eq(ast, newast) do
      replace(newast, pc)
    else
      newast
    end
  end

  defp repl(ast, pc) do
    case ast do
      {:unit, uv, ut} ->
        {:unit, replace(uv, pc), replace(ut, pc)}

      {:vari, var} ->
        Map.get(pc, {:vari, var}, {:vari, var})

      {:minus, a} ->
        {:minus, repl(a, pc)}

      {:numb, _, _} ->
        ast

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
          |> Enum.filter(fn el -> elem(el, 0) == :fcall end)
          |> Enum.filter(fn el -> elem(el, 1) == name and length(elem(el, 2)) == arity end)

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

  # Changes all numbers to Decimal struct
  defp walkn(ast) do
    case ast do
      {:numb, n} ->
        S.mknum(n * 1000, 1000)

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

      {:elev, base, exp} ->
        {:elev, walkn(base), walkn(exp)}

      {:deriv, f, v} ->
        {:deriv, walkn(f), walkn(v)}

      {:integ, f, v} ->
        {:integ, walkn(f), walkn(v)}

      {:unit, v, t} ->
        {:unit, walkn(v), walkn(t)}

      other ->
        throw("Unknown in walkn #{Exun.UI.tostr(other)}")
    end
  end

  @doc """
  Get a list of variables down a tree, checking cyclic definition
  from 'context' user defs.
  Check cyclic definitions in 'context', a map that holds
  values like %{"f" => "x^2+2*x+3"}. If you pass a cyclic map
  this function detects it, for example:
  ```
  %{"a"=>"b", "b"=>"c", "c"=>"a"}
  ```
  """
  def check_cyclic(context) do
    invalid_defs = cyc_check_definitions(context)

    if length(invalid_defs) > 0 do
      {:err, invalid_defs}
    else
      context
      |> cyc_maps_all()
      |> cyc_check_expand(MapSet.new())
    end
  end

  defp cyc_check_definitions(context) do
    for {name, _definition} <- context do
      with {:ok, tok, _} <- :exun_lex.string(String.to_charlist(name)),
           {:ok, tree} <- :exun_yacc.parse(tok) do
        {name, tree |> walkn()}
      end
    end
    |> Enum.reduce([], fn {v, t}, acc ->
      case t do
        {:vari, _} ->
          acc

        {:fcall, _, _} ->
          acc

        _ ->
          ["Invalid definiton for #{v}" | acc]
      end
    end)
  end

  defp cyc_check_expand(maps, prev_maps) do
    newmaps =
      for {varname, depends} <- maps, into: %{} do
        {varname,
         for depend <- depends do
           depend_depends = maps |> Map.get(depend)

           if depend_depends != nil do
             depend_depends |> MapSet.to_list()
           else
             depend
           end
         end
         |> List.flatten()
         |> MapSet.new()}
      end

    case newmaps
         |> Enum.reduce({:ok, nil}, fn {var, depends}, {cycle, msg} ->
           if MapSet.member?(depends, var) do
             {:err, "Cyclic definition var #{var}"}
           else
             {cycle, msg}
           end
         end) do
      {:ok, nil} ->
        cond do
          maps == newmaps ->
            {:ok, newmaps}

          prev_maps |> MapSet.member?(newmaps) ->
            {:err, "Cyclic definitions on user context", newmaps}

          maps != newmaps ->
            cyc_check_expand(newmaps, prev_maps |> MapSet.put(newmaps))
        end

      {:err, msg} ->
        {:err, msg, newmaps}
    end
  end

  defp cyc_maps_all(context) do
    for {var, def} <- context, into: %{} do
      with {:ok, tok, _} <- :exun_lex.string(def |> String.to_charlist()),
           {:ok, tree} <- :exun_yacc.parse(tok) do
        {var, cyc_extract_vars(tree |> walkn(), MapSet.new())}
      end
    end
  end

  defp cyc_extract_vars({_op, l, r}, acu) do
    MapSet.union(
      cyc_extract_vars(l, acu),
      cyc_extract_vars(r, acu)
    )
  end

  defp cyc_extract_vars({:vari, var}, acu) do
    MapSet.put(acu, var)
  end

  defp cyc_extract_vars(_a, acu) do
    acu
  end
end
