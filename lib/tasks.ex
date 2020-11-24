defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    e = Exun.new("1+1/x")
    f = Exun.Collect.expand_rec(e)
  end
end
