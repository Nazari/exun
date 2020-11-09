defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    e1 = Exun.new "a^2-b^2"
    Exun.eval e1
  end
end
