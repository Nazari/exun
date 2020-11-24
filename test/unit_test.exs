defmodule UnitTest do
  use ExUnit.Case

  test "1[1/h^2]" do
    ast = Exun.new("1[1/h^2]").ast
    assert Exun.Unit.toSI(ast) |> Exun.UI.tostr() == "7.71604938271605e-8[1/s^2]"
  end

  test "1[slug/N]" do
    assert Exun.Unit.toSI(Exun.new("1[slug/N]").ast) |> Exun.UI.tostr() ==
             "143.11732813057753[s^2/m]"
  end

  test "1[m]+3[cm]+2[dm]+4[mm]" do
    assert Exun.eval2str("1[m]+3[cm]+2[dm]+4[mm]") == "1.2340000000000002[m]"
  end

  test "(3[Kg] + 2[slug]) / (23[g] + 16[lb])" do
    assert Exun.eval2str("(3[Kg] + 2[slug]) / (23[g] + 16[lb])") == "4.421111667130774"
  end

  test "1[mm]/2+1[m]/2" do
    assert Exun.eval2str("1[mm]/2+1[m]/2") == "0.5005[m]"
  end

  test "Parse x[me]/3[se] with %{x => \"7\", me => m^2, se => s^2}" do
    tree =
      Exun.new(
        "x[me]/3[se]",
        %{"x" => "7", "me" => "m^2", "se" => "s^2"}
      ).ast

    assert tree == {
             {:m, :mult},
             [
               {
                 :elev,
                 {:unit, {:numb, 3, 1}, {:elev, {:vari, "s"}, {:numb, 2, 1}}},
                 {:numb, -1, 1}
               },
               {:unit, {:numb, 7, 1}, {:elev, {:vari, "m"}, {:numb, 2, 1}}}
             ]
           }
  end
end
