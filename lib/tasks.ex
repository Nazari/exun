defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.eval "f(y,3)", %{"f(a,b)"=>"a^2+a*b+b^2"}
  end
end
