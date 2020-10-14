defmodule Exun.Cyclic do
  @moduledoc """
  Check cyclic definitions in context
  """
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

  def check(context) do
    invalid_defs = check_definitions(context)

    if length(invalid_defs) > 0 do
      {:err, invalid_defs}
    else
      context
      |> maps_all()
      |> check_expand(MapSet.new())
    end
  end

  defp check_definitions(context) do
    for {name, _definition} <- context do
      with {:ok, tok, _} <- :exun_lex.string(String.to_charlist(name)),
           {:ok, tree} <- :exun_yacc.parse(tok) do
        {name, tree}
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

  defp check_expand(maps, prev_maps) do
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
