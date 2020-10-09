defmodule ExunTest do
  use ExUnit.Case
  alias Exun.Collect

  doctest Exun

  test "[ precedes sum" do
    assert Exun.eval("1[m]+1[cm]+1[mm]") in [ "1.011[m]", "101.1[cm]"]
  end

  test "[ precedes *" do
    assert Exun.eval("1[m]*1[cm]/2[m]") == "0.5[cm]"
  end

  test "Parse x[me]/3[se]" do
    assert Exun.parse("x[me]/3[se]") ==
      {:divi, {:unit, {:vari, "x"}, {:vari, "me"}},
      {:unit, {:numb, 3}, {:vari, "se"}}}
  end

  test "Parse x[me]/3[se] with %{x => \"7\", me => m^2, se => s^2}" do
    {tree, _} =
      Exun.parse(
        "x[me]/3[se]",
        %{"x" => "7", "me" => "m^2", "se" => "s^2"}
      )

    assert tree ==
             {:divi, {:unit, {:vari, "x"}, {:vari, "me"}}, {:unit, {:numb, 3}, {:vari, "se"}}}
  end

  test "1[1/h^2]" do
    u1 = Exun.parse("1[1/h^2]")
    assert Exun.Unit.to_si(u1) == {7.71604938271605e-8, %{"s" => -2}}
  end

  test "1[slug/N]" do
    u2 = Exun.parse("1[slug/N]")
    assert Exun.Unit.to_si(u2) == {143.11732813057753, %{"g" => 0, "m" => -1, "s" => 2}}
  end

  test "1[m]+3[cm]+2[dm]+4[mm]" do
    assert Exun.eval("1[m]+3[cm]+2[dm]+4[mm]") == "1.234[m]"
  end

  test "(3[kg] + 2[slug]) / (23[g] + 16[lb])" do
    u5 = Exun.eval("(3[kg] + 2[slug]) / (23[g] + 16[lb])")
    assert u5 == {:unit, {:numb, 4.046372279401674}, {:divi, {:vari, "N"}, {:vari, "g"}}}
  end

  test "1[m] * 1[cm]" do
    u6 = Exun.parse("1[m] * 1[cm]", %{})

    assert u6 == {{:mult, {:unit, {:numb, 1}, {:vari, "m"}}, {:unit, {:numb, 1}, {:vari, "cm"}}}, %{}}
    assert Exun.Unit.to_si(u6) == {0.01, %{"m" => 2}}
  end
end
