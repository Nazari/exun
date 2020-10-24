defmodule Mix.Tasks.Debug do
  use Mix.Task
  import Exun.Pattern
  import Exun

  def run(_) do
    umatch "f*g'x","sin(x)*cos(x)"
  end
end
