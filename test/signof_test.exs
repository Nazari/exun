defmodule SignofTest do
  use ExUnit.Case
  import Exun.Simpl

  test "so Number" do
    assert signof({:numb, 1, 1})
    assert signof({:numb, -1, -1})
    assert signof({:numb, 0, 1})
    assert !signof({:numb, -1, 1})
    assert !signof({:minus, {:numb, 1, 1}})
  end

  test "so " do
    assert signof({:elev, {:numb, -1, 1}, {:numb, 2, 1}})
  end
end
