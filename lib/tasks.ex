defmodule Mix.Tasks.Debg do
  use Mix.Task
  import Exun
  import Exun.Collect

  def run(_) do
    e = new("a/b+c*d/e+f/g+h")
    {_, l} = e.ast
    find_op(l, :mult_den)
  end
end
