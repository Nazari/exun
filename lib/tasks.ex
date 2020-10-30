defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.eval_ast "$3*x^2,x"
  end
end
