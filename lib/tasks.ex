defmodule Mix.Tasks.Debg do
  use Mix.Task

  def run(_) do
    Exun.Simpl.normalize({:minus, {{:m, :suma}, [{:numb, -3, 1}, {:vari, "x"}, {:minus, {:vari, "y"}}]}})

  end
end
