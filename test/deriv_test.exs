defmodule DerivTest do
  use ExUnit.Case

  test "der 1" do
    assert Exun.eval2str("sin(f(x))'x") == "cos(f(x))*f(x)'x"
  end

  test "der 2" do
    assert Exun.eval2str("(x^2+x*y+y^2)'x'y") == "1"
  end

end
