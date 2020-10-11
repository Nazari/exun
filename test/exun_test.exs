defmodule ExunTest do
  use ExUnit.Case

  doctest Exun

  test "[ precedes sum" do
    assert Exun.eval("1[m]+1[cm]+1[mm]") in [ "1.011[m]", "101.1[cm]"]
  end

  test "[ precedes *" do
    assert Exun.eval("1[m]*1[cm]/2[m]") == "0.005[m]"
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
    assert Exun.eval("(3[kg] + 2[slug]) / (23[g] + 16[lb])") == "4.421111667130775"

  end

  test "1[mm]/2+1[m]/2" do
    assert Exun.eval("1[mm]/2+1[m]/2") == "0.5005[m]"
  end

  test "Powers" do
    assert Exun.eval("(a+1)*(a+1)/(a+1)^3") == "1/(1+a)"
  end

  test "(a+1)^2/b, %{b=>a+1}" do
    assert Exun.eval("(a+1)^2/b", %{"b"=>"a+1"}) == "1+a"
  end

  test "(a+1)^2/b, %{b=>a+1,a=>2}" do
    assert Exun.eval("(a+1)^2/b", %{"b"=>"a+1","a"=>"2"}) == "3"
  end

  test "Order of sum" do
    assert Exun.eval("(1+a)*(a+1)") == "(1+a)^2"
  end
end
