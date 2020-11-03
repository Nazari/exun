defmodule Exun.Isol do
  @moduledoc """
  Isolation module. Try to isolate an ast from other AST.
  """
  import Exun.Fun
  import Exun.Math
  import Exun.Collect
  import Exun.Eq

  @zero {:numb, 0, 1}
  @doc """
  Return a list of tuples of form {:ok, solution} where olution is the isolated tree for original ast
  May be return more than one, if we can not collect ast in a single place of eqzero.
  """
  def isol({:equal, left, right}, ast) do
    eqzero = coll(rest(left, right))

    downtrace(eqzero, ast, @zero)
    |> Enum.reject(fn {res, _} -> res == :ko end)
    |> Enum.map(fn {res, sol} -> {res, coll(sol)} end)
  end

  defp downtrace(expr, ast, right) do
    case expr do
      ^ast ->
        [{:ok, right}]

      {:minus, a} ->
        downtrace(a, ast, {:minus, right})

      {:deriv, f, v} ->
        downtrace(f, ast, {:integ, right, v})

      {:integ, f, v} ->
        downtrace(f, ast, {:deriv, right, v})

      {:elev, a, b} ->
        try_base = downtrace(a, ast, elev(right,chpow(b)))
        try_expo = downtrace(b, ast, divi(ln(right),ln(a)))
        try_base ++ try_expo

      {:fcall, name, args} ->
        case finv(name) do
          nil ->
            [{:ko, right}]

          inv ->
            Enum.reduce(args, [], fn opand, ac ->
              downtrace(opand, ast, {:fcall, inv, [right]}) ++ ac
            end)
        end

      {{:m, op}, opands} ->
        rev = if op == :suma, do: &chsign/1, else: &chpow/1

        Enum.reduce(opands, [], fn opand, acu ->
          newlist =
            List.delete(opands, opand)
            |> Enum.map(rev)
            |> List.insert_at(0, right)
            |> Enum.sort(&smm/2)

          downtrace(opand, ast, {{:m, op}, newlist}) ++ acu
        end)

      _ ->
        [{:ko, right}]
    end
  end

  @doc """
  Find a subtree (fnd) all around tree and substitute it by rpl
  Simple exercise, not used for now in lib; I love the simplicity of
  Elixir for that kind of problems
  """
  def find_repl(tree, fnd, rpl) do
    case tree do
      ^fnd ->
        rpl

      {{:m, op}, l} ->
        {{:m, op}, Enum.map(l, &find_repl(&1, fnd, rpl))}

      {:fcall, f, args} ->
        {:fcall, f, Enum.map(args, &find_repl(&1, fnd, rpl))}

      {op, arg} ->
        {op, find_repl(arg, fnd, rpl)}

      {op, l, r} ->
        {op, find_repl(l, fnd, rpl), find_repl(r, fnd, rpl)}

      _ ->
        tree
    end
  end
end
