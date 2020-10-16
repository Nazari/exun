defmodule Exun.MProc do
  @moduledoc """
  Exun is intense in tuple management and recursion. This module
  is a try to buil tuple's element in parallel using Task. The macro mp
  is responsible for that.
  """

  @doc """
  Build tuple  in async mode, so every element of the tuple
  is spawned in its own process. Only for tuple sizes greater than 2

  For example:

  :timer.tc(fn -> {:timer.sleep(1000), :timer.sleep(1000), :timer.sleep(1000)} end)
  {3002269, {:ok, :ok, :ok}}

  :timer.tc(fn -> mp {:timer.sleep(1000), :timer.sleep(1000), :timer.sleep(1000)} end)
  {1001001, {:ok, :ok, :ok}}
  """
  defmacro parallel({:{}, _attrs, lista}) do
    task_list =
      lista
      |> Enum.map(fn el ->
        {{:., [], [{:__aliases__, [alias: false], [:Task]}, :async]}, [],
         [{:fn, [], [{:->, [], [[], el]}]}]}
      end)

    quote do
      unquote(task_list) |> Task.await_many() |> List.to_tuple()
    end
  end

  defmacro mp({a, b}), do: {a, b}
  defmacro mp({a}), do: {a}
end
