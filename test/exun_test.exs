defmodule ExunTest do
  use ExUnit.Case
  doctest Exun

  test "Parse x[me]/3[se]" do
    assert Exun.parse("x[me]/3[se]") ==
               {{:divi, {:unit, {:vari, "x"}, {:vari, "me"}}, {:unit, {:numb, 3}, {:vari, "se"}}},
                %{}}

  end

  test "Parse x[me]/3[se] with %{x => 7, me => m^2, se => s^2}" do
    {tree, _expand} =
      Exun.parse(
        "x[me]/3[se]",
        %{"x" => 7, "me" => "m^2", "se" => "s^2"}
      )

    assert tree ==  {:divi,
                      {:unit, {:numb, 7}, {:elev, {:vari, "m"}, {:numb, 2}}},
                      {:unit, {:numb, 3}, {:elev, {:vari, "s"}, {:numb, 2}}}
                    }
  end
end
