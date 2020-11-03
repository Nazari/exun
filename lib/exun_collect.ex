defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Simpl
  alias Exun.Math
  alias Exun.Eq

  @doc """
  Main collecting function. Try to simplify tree withou chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> Simpl.mkrec()
      |> expand_rec()
      |> Simpl.mkrec()

    if Eq.eq(newtree, tree), do: newtree, else: coll(newtree)
  end

  @doc """
  Expand mult(sum) to try collect more
  """
  def expand_rec(ast) do
    newast = expand(ast)

    if ast == newast do
      newast
    else
      expand_rec(newast)
    end
  end

  defp expand(ast) do
    case ast do
      {:numb, _, _} ->
        ast

      {:vari, _} ->
        ast

      {:deriv, f, v} ->
        {:deriv, expand(f), v}

      {:integ, f, v} ->
        {:integ, expand(f), v}

      {:fcall, f, args} ->
        {:fcall, f, Enum.map(args, &expand(&1))}

      {:unit, n, t} ->
        {:unit, expand(n), t}

      {:minus, a} ->
        {:minus, expand(a)}

      {:elev, a, {:numb, n, d}} when n > 0 and d == 1 and floor(n) == n ->
        {{:m, :mult}, List.duplicate(a, floor(n))}

      {:elev, b, e} ->
        {:elev, expand(b), expand(e)}

      {{:vector, s}, l} ->
        {{:vector, s}, l |> Enum.map(&expand/1)}

      {{t, rs, cs}, list, mr, mc} ->
        {{t, rs, cs}, list |> Enum.map(&expand/1), mr, mc}

      {{:m, :suma}, l} ->
        {{:m, :suma}, Enum.map(l, &expand(&1))}

      {{:m, :mult}, l} ->
        tsuma = List.keyfind(l, {:m, :suma}, 0)

        if tsuma != nil do
          {_, lsuma} = tsuma
          # Convert {:m,:mult} to {:m,:suma}
          remain = {{:m, :mult}, List.delete(l, tsuma)}
          {{:m, :suma}, Enum.map(lsuma, &Math.mult(remain, &1))}
        else
          {{:m, :mult}, Enum.map(l, &expand(&1))}
        end

      unknown ->
        throw("Unknown at collect: #{Exun.UI.tostr(unknown)}")
    end
  end
end
