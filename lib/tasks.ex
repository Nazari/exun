defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.eval2str("0.5*-(((x-1)^0.5)/(((1+x)^0.5)))-(0.5*((1+x)^0.5)*-(((x-1)^0.5)/-(((x-1)))))-(x/-((((x^2-1)^0.5))))")
  end
end
