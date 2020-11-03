defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.eval "$ln(f(x)),x"
  end
end
