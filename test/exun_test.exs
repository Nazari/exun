defmodule ExunTest do
  use ExUnit.Case

  doctest Exun

  test "[ precedes sum" do
    assert Exun.eval("1[m]+1[cm]+1[mm]") in ["1.011[m]", "101.1[cm]"]
  end

  test "[ precedes *" do
    assert Exun.eval("1[m]*1[cm]/2[m]") == "0.005[m]"
  end

  test "Parse x[me]/3[se]" do
    assert Exun.parse("x[me]/3[se]") ==
             {{{:m, :mult},
               [
                 {:elev, {:unit, {:numb, 3, 1}, {:vari, "se"}}, {:numb, -1, 1}},
                 {:unit, {:vari, "x"}, {:vari, "me"}}
               ]}, %{}}
  end

  test "Powers" do
    assert Exun.eval("(a+1)*(a+1)/(a+1)^3") == "1/(1+a)"
  end

  test "(a+1)^2/b, %{b=>a+1}" do
    assert Exun.eval("(a+1)^2/b", %{"b" => "a+1"}) == "1+a"
  end

  test "(a+1)^2/b, %{b=>a+1,a=>2}" do
    assert Exun.eval("(a+1)^2/b", %{"b" => "a+1", "a" => "2"}) == "3"
  end

  test "Order of sum" do
    assert Exun.eval("(1+a)*(a+1)") == "(1+a)^2"
  end

  test "Context" do
    assert Exun.eval("(a+b)^2/c", %{"a" => "20[m]", "b" => "2[cm]", "c" => "3[s^2]"}) ==
             "133.60013333333333[m^2/s^2]"
  end

  test "Sort of tree" do
    assert Exun.eval("(1+a)*(a+1)/(a+1)^3") == "1/(1+a)"
  end

  test "Convert unit" do
    assert Exun.Unit.convert("70[mi/h]", "Km/h") == "112.65408000000001[Km/h]"
  end

  test "Factorize" do
    assert Exun.Unit.factorize("1[A*Kg*m/s^2]", "[slug*cm]") == "6.852176585682165[cm*slug*A/s^2]"
  end

  test "Param substitution" do
    assert Exun.eval("f(y,3)", %{"f(a,b)"=>"a^2+a*b+b^2"}) == "9+3*y+y^2"
  end
end
