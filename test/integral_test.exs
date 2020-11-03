defmodule IntegralTest do
  use ExUnit.Case

  test "Integral ln" do
    assert Exun.eval("$ln(f(x)),x") == "x*ln(f(x))-($(x/f(x)),x)"
  end

  test "Integral poly" do
    assert Exun.eval("$1+2*x+3*x^2+4*x^3,x") == "x*(1+x*(1+x*(1+x)))"
  end

  test "Integral type U * U'x" do
    assert Exun.eval("$sin(x)*cos(x),x") == "0.5*sin(x)^2"
  end

  test "Match integral of product" do
    assert Exun.Pattern.match("f'x", "2*x", %{}) == [
             ok: %{
               {:deriv, {:vari, "f"}, {:vari, "x"}} =>
                 {{:m, :mult}, [{:numb, 2.0, 1.0}, {:vari, "x"}]}
             }
           ]
  end

  test "Integrals" do
    [
      "ln",
      "sin",
      "cos",
      # "tan",
      # "asin",
      # "acos",
      # "atan",
      "sinh",
      "cosh"
      # "atanh",
      # "asinh"
      # "acosh",
      # "atanh"
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
end
