defmodule PatternTest do
  use ExUnit.Case
  import Exun.Pattern

  test "Patterns 01" do
    assert match("f(g)*g'x", "cos(sin(x))*cos(x)", %{}) == [
             ok: %{
               {:vari, "g"} => {:fcall, "sin", [vari: "x"]},
               {:deriv, {:vari, "g"}, {:vari, "x"}} => {:fcall, "cos", [vari: "x"]},
               {:fcall, "f", [vari: "g"]} => {:fcall, "cos", [{:fcall, "sin", [vari: "x"]}]}
             }
           ]
  end

  test "Patterns 02" do
    assert(
      match("f*g", "sin(x)/cos(x)", %{}) == [
        {
          :ok,
          %{
            {:vari, "f"} => {
              {:m, :mult},
              [{:elev, {:fcall, "cos", [vari: "x"]}, {:minus, {:fcall, "sin", [vari: "x"]}}}, {:fcall, "sin", [vari: "x"]}]
            },
            {:vari, "g"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}}
          }
        },
        {
          :ok,
          %{
            {:vari, "f"} => {{:m, :mult}, [{:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}}, {:fcall, "sin", [vari: "x"]}]},
            {:vari, "g"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:minus, {:fcall, "sin", [vari: "x"]}}}
          }
        },
        {
          :ok,
          %{
            {:vari, "f"} => {{:m, :mult}, [{:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1, 1}}, {:fcall, "sin", [vari: "x"]}]},
            {:vari, "g"} => {:numb, 1, 1}
          }
        },
        {
          :ok,
          %{
            {:vari, "f"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:minus, {:fcall, "sin", [vari: "x"]}}},
            {:vari, "g"} => {{:m, :mult}, [{:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}}, {:fcall, "sin", [vari: "x"]}]}
          }
        },
        {
          :ok,
          %{
            {:vari, "f"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}},
            {:vari, "g"} => {
              {:m, :mult},
              [{:elev, {:fcall, "cos", [vari: "x"]}, {:minus, {:fcall, "sin", [vari: "x"]}}}, {:fcall, "sin", [vari: "x"]}]
            }
          }
        },
        {:ok, %{{:vari, "f"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1, 1}}, {:vari, "g"} => {:fcall, "sin", [vari: "x"]}}},
        {:ok, %{{:vari, "f"} => {:fcall, "sin", [vari: "x"]}, {:vari, "g"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1, 1}}}},
        {:ok, %{{:vari, "f"} => {:fcall, "sin", [vari: "x"]}, {:vari, "g"} => {:numb, 1, 1}}},
        {
          :ok,
          %{
            {:vari, "f"} => {:numb, 1, 1},
            {:vari, "g"} => {{:m, :mult}, [{:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1, 1}}, {:fcall, "sin", [vari: "x"]}]}
          }
        },
        {:ok, %{{:vari, "f"} => {:numb, 1, 1}, {:vari, "g"} => {:fcall, "sin", [vari: "x"]}}}
      ]
    )
  end

  test "Patterns 03" do
    assert match("u*v'x", "x*cos(x)", %{}) == [
             {:ok,
              %{
                {:vari, "u"} => {:vari, "x"},
                {:deriv, {:vari, "v"}, {:vari, "x"}} => {:fcall, "cos", [vari: "x"]}
              }},
             {:ok,
              %{
                {:vari, "u"} => {{:m, :mult}, [{:fcall, "cos", [vari: "x"]}, {:vari, "x"}]},
                {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 1, 1}
              }},
             {:ok,
              %{
                {:vari, "u"} => {:fcall, "cos", [vari: "x"]},
                {:deriv, {:vari, "v"}, {:vari, "x"}} => {:vari, "x"}
              }},
             {:ok,
              %{
                {:vari, "u"} => {:numb, 1, 1},
                {:deriv, {:vari, "v"}, {:vari, "x"}} =>
                  {{:m, :mult}, [{:fcall, "cos", [vari: "x"]}, {:vari, "x"}]}
              }}
           ]
  end

  test "Patterns 04" do
    assert match("u*v'x", "x", %{}) == [
             ok: %{
               {:vari, "u"} => {:vari, "x"},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 1, 1}
             },
             ok: %{
               {:vari, "u"} => {:numb, 1, 1},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:vari, "x"}
             }
           ]
  end

  test "Patterns 05" do
    assert match("g(y)+f'x", "1+x+y", %{}) == [
      ok: %{{:vari, "y"} => {:vari, "x"}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1, 1}, {:fcall, "g", [vari: "y"]} => {:vari, "x"}},
      ok: %{{:vari, "y"} => {:vari, "y"}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"}, {:fcall, "g", [vari: "y"]} => {:vari, "y"}},
      ok: %{{:vari, "y"} => {:vari, "y"}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1, 1}, {:fcall, "g", [vari: "y"]} => {:vari, "x"}},
      ok: %{{:vari, "y"} => {:numb, 0, 1}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"}, {:fcall, "g", [vari: "y"]} => {:vari, "y"}},
      ok: %{{:vari, "y"} => {:numb, 1, 1}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"}, {:fcall, "g", [vari: "y"]} => {:vari, "y"}},
      ok: %{{:vari, "y"} => {:numb, 1, 1}, {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1, 1}, {:fcall, "g", [vari: "y"]} => {:vari, "x"}}
    ]
  end

  test "Patterns 06" do
    assert match("f(x)", "sin(x)", %{}) == [
             ok: %{
               {:vari, "x"} => {:vari, "x"},
               {:fcall, "f", [vari: "x"]} => {:fcall, "sin", [vari: "x"]}
             }
           ]
  end

  test "Patterns 07" do
    assert match("f(2*x)", "sin(2*x)", %{}) == [
             ok: %{
               {:fcall, "f", [{{:m, :mult}, [{:numb, 2, 1}, {:vari, "x"}]}]} =>
                 {:fcall, "sin", [{{:m, :mult}, [{:numb, 2, 1}, {:vari, "x"}]}]},
               {:vari, "x"} => {:vari, "x"}
             }
           ]
  end

  test "Pattern 08" do
    assert match("u*v'x", "2*x^2", %{}) == [
             {
               :ok,
               %{
                 {:vari, "u"} =>
                   {{:m, :mult}, [{:numb, 2, 1}, {:elev, {:vari, "x"}, {:numb, 2, 1}}]},
                 {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 1, 1}
               }
             },
             {:ok,
              %{
                {:vari, "u"} => {:elev, {:vari, "x"}, {:numb, 2, 1}},
                {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 2, 1}
              }},
             {
               :ok,
               %{
                 {:vari, "u"} => {:numb, 1, 1},
                 {:deriv, {:vari, "v"}, {:vari, "x"}} =>
                   {{:m, :mult}, [{:numb, 2, 1}, {:elev, {:vari, "x"}, {:numb, 2, 1}}]}
               }
             },
             {:ok,
              %{
                {:vari, "u"} => {:numb, 2, 1},
                {:deriv, {:vari, "v"}, {:vari, "x"}} => {:elev, {:vari, "x"}, {:numb, 2, 1}}
              }}
           ]
  end

  test "Pattern 09" do
    assert match("a*b+a*c", "x/((1-x^2)^0.5)+x*((1-x^2)^0.5)/((-1+x^2))", %{}) == [
      ok: %{
        {:vari, "a"} => {:vari, "x"},
        {:vari, "b"} => {:minus, {:elev, {:vari, "x"}, {:numb, -1, 1}}},
        {:vari, "c"} => {:minus, {:elev, {:vari, "x"}, {:numb, -1, 1}}}
      },
      ok: %{{:vari, "a"} => {:numb, 1, 1}, {:vari, "b"} => {:numb, -1, 1}, {:vari, "c"} => {:numb, -1, 1}},
      ok: %{{:vari, "a"} => {:numb, 1, 1}, {:vari, "b"} => {:numb, -1, 1}, {:vari, "c"} => {:numb, 0, 1}},
      ok: %{{:vari, "a"} => {:numb, 1, 1}, {:vari, "b"} => {:numb, 0, 1}, {:vari, "c"} => {:numb, -1, 1}}
    ]
  end
end
