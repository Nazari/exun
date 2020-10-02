defmodule Exun do
  alias Exun.Tree
  alias Exun.Cyclic

  def parse(txt) do
    parse(txt, %{})
  end

  def parse(txt, context) do
    Cyclic.check(context)
    with {:ok, toks, _} <- :exun_lex.string(txt |> String.to_charlist()),
         {:ok, utree} <- :exun_yacc.parse(toks) do
      Tree.expand(utree, context, %{})
    end
  end

  def get_var_exponents({:unit, _val, tree}) do
    rec_gvt(tree, %{}, 1)
  end

  defp rec_gvt({op, left, right}, map_var, sign) do
    map_var =
      case op do
        :divide ->
          map_var = rec_gvt(left, map_var, sign)
          rec_gvt(right, map_var, -sign)

        :multiply ->
          map_var = rec_gvt(left, map_var, sign)
          rec_gvt(right, map_var, sign)

        :power ->
          rec_gvt(left, map_var, sign * right)
      end

    map_var
  end

  defp rec_gvt({:variable, var}, map_var, sign) do
    current_exponent = map_var |> Map.get(var, 0)
    map_var |> Map.put(var, current_exponent + sign)
  end
end
