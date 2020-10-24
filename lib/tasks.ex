defmodule Mix.Tasks.Debg do
  use Mix.Task
  import Exun.Pattern

  def run(_) do
    umatch "f*g'x","sin(x)*cos(x)"
  end
end
