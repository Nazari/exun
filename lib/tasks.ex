defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.eval("$2*x^2,x")
  end
end
