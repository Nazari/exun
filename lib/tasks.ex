defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.Pattern.umatch "a+x*z-x*j","f+a*b/d-(a*c)"
  end
end
