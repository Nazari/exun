defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.Pattern.umatch("a*d+b*c","x*y+z*k" )
  end
end
