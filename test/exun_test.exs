defmodule ExunTest do
  use ExUnit.Case

  doctest Exun

  test "Integral poly" do
    assert Exun.eval("$1+2*x+3*x^2+4*x^3,x")=="x*(1+x*(1+x*(1+x)))"
  end

  test "Integral type U * U'x" do
    assert Exun.eval("$sin(x)*cos(x),x") == "0.5*sin(x)^2"
  end

  test "Match integral of product" do
    assert Exun.Pattern.match("f'x", "2*x", %{}) == [
             ok: %{
               {:vari, "f"} => {:elev, {:vari, "x"}, {:numb, 2}},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {{:m, :mult}, [numb: 2, vari: "x"]}
             }
           ]
  end

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
                 {:elev, {:unit, {:numb, 3}, {:vari, "se"}}, {:numb, -1}},
                 {:unit, {:vari, "x"}, {:vari, "me"}}
               ]}, %{}}
  end

  test "Parse x[me]/3[se] with %{x => \"7\", me => m^2, se => s^2}" do
    {tree, _} =
      Exun.parse(
        "x[me]/3[se]",
        %{"x" => "7", "me" => "m^2", "se" => "s^2"}
      )

    assert tree ==
             {{:m, :mult},
              [
                {:elev, {:unit, {:numb, 3}, {:vari, "se"}}, {:numb, -1}},
                {:unit, {:vari, "x"}, {:vari, "me"}}
              ]}
  end

  test "1[1/h^2]" do
    {ast, _ctx} = Exun.parse("1[1/h^2]")
    assert Exun.Unit.toSI(ast) |> Exun.UI.tostr() == "7.71604938271605e-8[1/s^2]"
  end

  test "1[slug/N]" do
    {u2, _ctx} = Exun.parse("1[slug/N]")
    assert Exun.Unit.toSI(u2) |> Exun.UI.tostr() == "143.11732813057753[s^2/m]"
  end

  test "1[m]+3[cm]+2[dm]+4[mm]" do
    assert Exun.eval("1[m]+3[cm]+2[dm]+4[mm]") == "1.234[m]"
  end

  test "(3[Kg] + 2[slug]) / (23[g] + 16[lb])" do
    assert Exun.eval("(3[Kg] + 2[slug]) / (23[g] + 16[lb])") == "4.421111667130774"
  end

  test "1[mm]/2+1[m]/2" do
    assert Exun.eval("1[mm]/2+1[m]/2") == "0.5005[m]"
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
             "133.6001333333333[m^2/s^2]"
  end

  test "Sort of tree" do
    assert Exun.eval("(1+a)*(a+1)/(a+1)^3") == "1/(1+a)"
  end

  test "der 1" do
    assert Exun.eval("sin(f(x))'x") == "cos(f(x))*f(x)'x"
  end

  test "der 2" do
    assert Exun.eval("(x^2+x*y+y^2)'x'y") == "1"
  end

  test "Convert unit" do
    assert Exun.Unit.convert("70[mi/h]", "Km/h") == "112.65408[Km/h]"
  end

  test "Factorize" do
    assert Exun.Unit.factorize("1[A*Kg*m/s^2]", "[slug*cm]") == "6.852176585682165[slug*cm*A/s^2]"
  end

  test "Integrals" do
    [
      "ln",
      "sin",
      "cos",
      "tan",
      #"asin",
      #"acos",
      #"atan",
      "sinh",
      "cosh",
      #"atanh",
      "asinh",
      #"acosh",
      #"atanh"
    ]
    |> Enum.map(fn name ->
      integ_fun = "$#{name}(x),x"
      result_integ = Exun.eval(integ_fun)
      deriv_fun = "(#{result_integ})'x"
      result_deriv_ast = Exun.eval_ast(deriv_fun)
      reverted = Exun.Fun.revert_compounds(result_deriv_ast)

      result_deriv =
        if reverted == nil do
          Exun.UI.tostr(result_deriv_ast)
        else
          Exun.UI.tostr(reverted)
        end

      assert name <> "(x)" == result_deriv
    end)
  end

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
      match("h+f*g", "sin(x)/cos(x)", %{}) == [
        ok: %{
          {:vari, "f"} => {:numb, 0},
          {:vari, "g"} => {:numb, 1},
          {:vari, "h"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1}}
             ]}
        },
        ok: %{
          {:vari, "f"} => {:numb, 1},
          {:vari, "g"} => {:numb, 0},
          {:vari, "h"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1}}
             ]}
        },
        ok: %{
          {:vari, "f"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]},
                {{:m, :suma}, [numb: -1, minus: {:fcall, "sin", [vari: "x"]}]}}
             ]},
          {:vari, "g"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}},
          {:vari, "h"} => {:numb, 0}
        },
        ok: %{
          {:vari, "f"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}}
             ]},
          {:vari, "g"} =>
            {:elev, {:fcall, "cos", [vari: "x"]},
             {{:m, :suma}, [numb: -1, minus: {:fcall, "sin", [vari: "x"]}]}},
          {:vari, "h"} => {:numb, 0}
        },
        ok: %{
          {:vari, "f"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1}},
          {:vari, "g"} => {:fcall, "sin", [vari: "x"]},
          {:vari, "h"} => {:numb, 0}
        },
        ok: %{
          {:vari, "f"} =>
            {:elev, {:fcall, "cos", [vari: "x"]},
             {{:m, :suma}, [numb: -1, minus: {:fcall, "sin", [vari: "x"]}]}},
          {:vari, "g"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}}
             ]},
          {:vari, "h"} => {:numb, 0}
        },
        ok: %{
          {:vari, "f"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:fcall, "sin", [vari: "x"]}},
          {:vari, "g"} =>
            {{:m, :mult},
             [
               {:fcall, "sin", [vari: "x"]},
               {:elev, {:fcall, "cos", [vari: "x"]},
                {{:m, :suma}, [numb: -1, minus: {:fcall, "sin", [vari: "x"]}]}}
             ]},
          {:vari, "h"} => {:numb, 0}
        },
        ok: %{
          {:vari, "f"} => {:fcall, "sin", [vari: "x"]},
          {:vari, "g"} => {:elev, {:fcall, "cos", [vari: "x"]}, {:numb, -1}},
          {:vari, "h"} => {:numb, 0}
        }
      ]
    )
  end

  test "Patterns 03" do
    assert match("u*v'x", "x*cos(x)", %{}) == [
             ok: %{
               {:vari, "u"} => {:vari, "x"},
               {:vari, "v"} => {:fcall, "sin", [vari: "x"]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:fcall, "cos", [vari: "x"]}
             },
             ok: %{
               {:vari, "u"} => {:fcall, "cos", [vari: "x"]},
               {:vari, "v"} => {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:vari, "x"}
             }
           ]
  end

  test "Patterns 04" do
    assert match("u*v'x", "x", %{}) == [
             ok: %{
               {:vari, "u"} => {:numb, 1},
               {:vari, "v"} => {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:vari, "x"}
             },
             ok: %{
               {:vari, "u"} => {:vari, "x"},
               {:vari, "v"} => {:vari, "x"},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 1}
             }
           ]
  end

  test "Patterns 05" do
    assert match("g(y)+f'x", "1+x+y", %{}) == [
             ok: %{
               {:vari, "f"} => {:vari, "x"},
               {:vari, "y"} => {:numb, 1},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [vari: "y", vari: "x"]}
             },
             ok: %{
               {:vari, "f"} => {:vari, "x"},
               {:vari, "y"} => {:vari, "x"},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [vari: "y", vari: "x"]}
             },
             ok: %{
               {:vari, "f"} => {:vari, "x"},
               {:vari, "y"} => {:vari, "y"},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [vari: "y", vari: "x"]}
             },
             ok: %{
               {:vari, "f"} => {:vari, "x"},
               {:vari, "y"} => {{:m, :suma}, [vari: "y", vari: "x"]},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:numb, 1},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [vari: "y", vari: "x"]}
             },
             ok: %{
               {:vari, "f"} => {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:vari, "y"} => {:numb, 1},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [numb: 1, vari: "y"]}
             },
             ok: %{
               {:vari, "f"} => {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:vari, "y"} => {:vari, "y"},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [numb: 1, vari: "y"]}
             },
             ok: %{
               {:vari, "f"} => {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:vari, "y"} => {{:m, :suma}, [numb: 1, vari: "y"]},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {:vari, "x"},
               {:fcall, "g", [vari: "y"]} => {{:m, :suma}, [numb: 1, vari: "y"]}
             },
             ok: %{
               {:vari, "f"} =>
                 {{:m, :suma},
                  [
                    {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
                    {:vari, "x"}
                  ]},
               {:vari, "y"} => {:numb, 0},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {{:m, :suma}, [numb: 1, vari: "x"]},
               {:fcall, "g", [vari: "y"]} => {:vari, "y"}
             },
             ok: %{
               {:vari, "f"} =>
                 {{:m, :suma},
                  [
                    {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
                    {:vari, "x"}
                  ]},
               {:vari, "y"} => {:numb, 1},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {{:m, :suma}, [numb: 1, vari: "x"]},
               {:fcall, "g", [vari: "y"]} => {:vari, "y"}
             },
             ok: %{
               {:vari, "f"} =>
                 {{:m, :suma},
                  [
                    {{:m, :mult}, [{:numb, 0.5}, {:elev, {:vari, "x"}, {:numb, 2}}]},
                    {:vari, "x"}
                  ]},
               {:vari, "y"} => {:vari, "y"},
               {:deriv, {:vari, "f"}, {:vari, "x"}} => {{:m, :suma}, [numb: 1, vari: "x"]},
               {:fcall, "g", [vari: "y"]} => {:vari, "y"}
             }
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
               {:vari, "x"} => {:vari, "x"},
               {:fcall, "f", [{{:m, :mult}, [numb: 2, vari: "x"]}]} =>
                 {:fcall, "sin", [{{:m, :mult}, [numb: 2, vari: "x"]}]}
             }
           ]
  end

  test "Pattern 08" do
    assert match("u*v'x", "2*x^2", %{}) == [
             ok: %{
               {:vari, "u"} => {:numb, 1},
               {:vari, "v"} =>
                 {{:m, :mult}, [{:numb, 0.6666666666666666}, {:elev, {:vari, "x"}, {:numb, 3}}]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} =>
                 {{:m, :mult}, [{:numb, 2}, {:elev, {:vari, "x"}, {:numb, 2}}]}
             },
             ok: %{
               {:vari, "u"} => {:numb, 2},
               {:vari, "v"} =>
                 {{:m, :mult}, [{:numb, 0.3333333333333333}, {:elev, {:vari, "x"}, {:numb, 3}}]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:elev, {:vari, "x"}, {:numb, 2}}
             },
             ok: %{
               {:vari, "u"} => {{:m, :mult}, [{:numb, 2}, {:elev, {:vari, "x"}, {:numb, 2}}]},
               {:vari, "v"} => {:vari, "x"},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 1}
             },
             ok: %{
               {:vari, "u"} => {:elev, {:vari, "x"}, {:numb, 2}},
               {:vari, "v"} => {{:m, :mult}, [numb: 2, vari: "x"]},
               {:deriv, {:vari, "v"}, {:vari, "x"}} => {:numb, 2}
             }
           ]
  end
end
