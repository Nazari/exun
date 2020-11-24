defmodule Exun.Collect do
  @moduledoc """
  Collect Math expression, try to simplify
  """
  alias Exun.Simpl, as: S
  alias Exun.Eq, as: E

  @doc """
  Main collecting function. Try to simplify tree withou chaging its value
  Gets and returns an AST, as produced by Exun.parse.
  """
  def coll(tree) when is_tuple(tree) do
    newtree =
      tree
      # |> IO.inspect(label: "make00, orig->mkrec")
      |> S.mkrec()
      |> expand_rec()
      |> S.mkrec()

    if E.eq(newtree, tree), do: newtree, else: coll(newtree)
  end

  def coll(%Exun{ast: ast, pc: pc}) do
    %Exun{ast: coll(ast), pc: pc}
  end

  @doc """
  Expand mult(sum) to try collect more
  """
  def expand_rec(ast) when is_tuple(ast) do
    newast = expand(ast)

    if ast == newast do
      newast
    else
      expand_rec(newast)
    end
  end

  def expand_rec(%Exun{ast: ast, pc: pc}) do
    %Exun{ast: expand_rec(ast), pc: pc}
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
        case a do
          {:vari, _} -> ast
          _ -> {{:m, :mult}, List.duplicate(a, floor(n))}
        end

      {:elev, a, {:numb, n, d}} when n < -1 and d == 1 and floor(n) == n ->
        case a do
          {:vari, _} -> ast
          _ -> {:elev, {{:m, :mult}, List.duplicate(a, floor(-n))}, {:numb, -1, 1}}
        end

      {:elev, b, e} ->
        {:elev, expand(b), expand(e)}

      {{:vector, s}, l} ->
        {{:vector, s}, l |> Enum.map(&expand/1)}

      {{t, rs, cs}, list, mr, mc} ->
        {{t, rs, cs}, list |> Enum.map(&expand/1), mr, mc}

      {{:m, :suma}, l} ->
        {{:m, :suma}, Enum.map(l, &expand_rec(&1))}
        #l = Enum.map(l, &expand_rec(&1))
        #find_op(l, :mult_den)

      {{:m, :mult}, l} ->
        l = Enum.map(l, &expand_rec/1)

        cond do
          (newop = find_op(l, :suma_num)) != nil ->
            newop

          (newop = find_op(l, :suma_den)) != nil ->
            newop

          true ->
            {{:m, :mult}, l}
        end

      unknown ->
        throw("Unknown at expand: #{Exun.UI.tostr(unknown)}")
    end
  end

  def find_op(list, :suma_num) do
    tsuma_n = List.keyfind(list, {:m, :suma}, 0)

    if tsuma_n != nil do
      {_, lsuma} = tsuma_n
      # Convert {:m,:mult} to {:m,:suma}
      remain = {{:m, :mult}, List.delete(list, tsuma_n)}
      {{:m, :suma}, Enum.map(lsuma, &S.mult(remain, &1))}
    else
      nil
    end
  end

  def find_op(list, :suma_den) do
    {sublist, remain} =
      Enum.reduce(list, {[], []}, fn op, {sl, re} ->
        case op do
          {:elev, {{:m, :suma}, _}, {:numb, n, 1}} when floor(n) == n and n < 0 ->
            {[op | sl], re}

          _ ->
            {sl, [op | re]}
        end
      end)

    if length(sublist) > 1 do
      denom = S.chpow(expand_rec(S.chpow({{:m, :mult}, sublist})))
      {{:m, :mult}, [denom | remain]}
    else
      nil
    end
  end

  # Try transform a+b/c+d/e to (ace+be+dc)/ce
  def find_op(list, :mult_den) do
    denoms =
      Enum.reduce(list, [], fn el, denoms ->
        case el do
          {{:m, :mult}, ml} ->
            Enum.reduce(ml, denoms, fn opand, denoms ->
              case opand do
                {:elev, a, b} ->
                  if not S.signof(b),
                    do: [S.normalize({:elev, a, S.chsign(b)}) | denoms],
                    else: denoms

                _ ->
                  denoms
              end
            end)

          {:elev, a, b} ->
            if not S.signof(b), do: [S.normalize({:elev, a, S.chsign(b)}) | denoms], else: denoms

          _ ->
            denoms
        end
      end)

    if length(denoms) == 0 do
      {{:m, :suma}, list}
    else
      lmdenoms =
        Enum.reduce(denoms, [], fn opand, ac -> S.add_opand(:mult, opand, {{:m, :mult}, ac}) end)
        #|> IO.inspect(label: "lmdenoms")

      lmnumer =
        Enum.map(list, fn el ->
          {{:m, :mult}, S.add_opand(:mult, el, {{:m, :mult}, lmdenoms})}
        end)
        #|> IO.inspect(label: "lmnumer")

      numer = S.mkrec({{:m, :suma}, lmnumer})
      #|>IO.inspect(label: "numer")
      denom = S.mkrec({{:m, :mult}, lmdenoms})
      #|> IO.inspect(label: "denom")
      {{:m, :mult}, [numer, {:elev, denom, {:numb, -1, 1}}]}
    end
  end
end
