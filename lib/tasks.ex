defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.Pattern.umatch "f(g)*g'x", "3*x^2*(x^3+1)^2",[],false
  end
end
