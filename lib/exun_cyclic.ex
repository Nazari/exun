defmodule Exun.Cyclic do
  @doc """
  Get a list of variables down a tree, checking cyclic definition
  from 'context' user defs

  """

  def check(context) do
    check_expand(maps_all(context), MapSet.new())
  end

  def check_expand(maps, prev_maps) do
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
            {:err, "Cyclic definitions", newmaps}

          maps != newmaps ->
            check_expand(newmaps, prev_maps |> MapSet.put(newmaps))
        end

      {:err, msg} ->
        {:err, msg, newmaps}
    end
  end

  defp maps_all(context) do
    for {var, def} <- context, into: %{} do
      with {:ok, tok, _} <- :exun_lex.string(def |> String.to_charlist()),
           {:ok, tree} <- :exun_yacc.parse(tok) do
        {var, extract_vars(tree, MapSet.new())}
      end
    end
  end

  defp extract_vars({_op, l, r}, acu) do
    MapSet.union(
      extract_vars(l, acu),
      extract_vars(r, acu)
    )
  end

  defp extract_vars({:vari, var}, acu) do
    MapSet.put(acu, var)
  end

  defp extract_vars(_a, acu) do
    acu
  end
end