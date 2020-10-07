defmodule ExunTest do
  use ExUnit.Case
  alias Exun.Tree
  doctest Exun

  test "[ precedes sum" do
    assert Exun.eval("1[m]+1[cm]+1[mm]") == "1.011[m]"
  end

  test "[ precedes *" do
    assert Exun.eval("1[m]*1[cm]/2[m]") == "0.5[cm]"
  end

  test "Parse x[me]/3[se]" do
    assert Exun.parse("x[me]/3[se]") |> Tree.reduce() ==
             {:unit, {:divi, {:vari, "x"}, {:numb, 3}}, {:divi, {:vari, "me"}, {:vari, "se"}}}
  end

  test "Parse x[me]/3[se] with %{x => \"7\", me => m^2, se => s^2}" do
    {tree, _} =
      Exun.parse(
        "x[me]/3[se]",
        %{"x" => "7", "me" => "m^2", "se" => "s^2"}
      )
      |> Tree.reduce()

    assert tree ==
             {:divi, {:unit, {:vari, "x"}, {:vari, "me"}}, {:unit, {:numb, 3}, {:vari, "se"}}}
  end

  test "SI conv, sum" do
    u1 = Exun.parse("1[1/hour^2]")
    assert Exun.Unit.to_si(u1) == {2.777777777777778e-4, %{"s" => -2}}

    u2 = Exun.parse("1[slug/N]")
    assert Exun.Unit.to_si(u2) == {143.0790533834911, %{"g" => 0, "m" => 0, "s" => 0}}

    assert Exun.eval("1[m] + 1[cm]") == "1.01[m]"

    u4 = Exun.parse("(2[slug] + 3[N]) / (16[lb] + 23[g])",%{})
    assert u4 == {:unit, {:numb, 4.046372279401674}, {:divi, {:vari, "slug"}, {:vari, "lb"}}}

    u5 = Exun.parse("(3[N] + 2[slug]) / (23[g] + 16[lb])", %{})
    assert u5 == {:unit, {:numb, 4.046372279401674}, {:divi, {:vari, "N"}, {:vari, "g"}}}

    u6 = Exun.parse("1[m] * 1[cm]", %{})
    assert u6 == {:unit, {:numb, 1}, {:mult, {:vari, "m"}, {:vari, "cm"}}}
    assert Exun.Unit.to_si(u6) == {0.01, %{"m" => 2}}
  end
end
