defmodule ExunTest do
  use ExUnit.Case
  doctest Exun

  test "Parse x[me]/3[se]" do
    assert Exun.parse("x[me]/3[se]") ==
             {{:unit, {:divi, {:vari, "x"}, {:numb, 3}}, {:divi, {:vari, "me"}, {:vari, "se"}}},
              %{}}
  end

  test "Parse x[me]/3[se] with %{x => \"7\", me => m^2, se => s^2}" do
    {tree, _expand} =
      Exun.parse(
        "x[me]/3[se]",
        %{"x" => "7", "me" => "m^2", "se" => "s^2"}
      )

    assert tree ==
      {:unit, {:divi, {:vari, "x"}, {:numb, 3}}, {:divi, {:vari, "me"}, {:vari, "se"}}}
  end
end
