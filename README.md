# Exun

Symbolic math library for Elixir, with unit support.
Alpha state.

TODO:
 - Multiprocess, make reductions in parallell via Tasks 
 - Temperature unit conversions
 - Integration, Summatory
 - Fractions, to avoid decimal ops
 - Define equations, not only expressions: Isolate variables
 - Add more testing and revise docs
 
DONE:
 + Derivate
 + Units (factorize, conversion, operation, user definition)
 + Context definition for vars and funcs
 + Functions and User functions  

run "iex -S mix" inside exun dir and type:
```
import Exun
import Exun.Unit

eval "(1+a)*(a+1)/(a+1)^3"
"1/(1+a)"

eval "1[m]+1[cm]"
"1.01[m]"

factorize "1[A*Kg*m/s^2]","[slug*cm]"
"6.852176585682164[slug*cm*A/s^2]"

"120[Km/h]" |> convert("m/s")
"33.3333333333[m/s]"
```

Call Exun.Unit.help for a list of supported units, you can also add new units via context (a map that holds definitions you can use inside expression)
```
eval "25[Km/h]+14[myunit^2]", %{ "myunit" => "(m/s)^0.5" }
"20.94444444444444[m/s]"
```


You can put 'context' also, passing a map that defines values for variables:
```
eval "(a+1)^2/b", %{"b"=>"a+1"}
"a+1"

eval "(a+b)^2/c", %{"a"=>"20[m]","b"=>"2[cm]","c"=>"3[s^2]"}
"133.60013333333333[m^2/s^2]"
```

Version 0.1.2 can derivate and support some functions (trigonometrics, hyperbolics, ln):
Operator ' is derivate, so "f'x" is df(x)/dx
```
eval "(1+x)^2'x"
"2*(1+x)"

eval "sin(2*x)'x"
"2*cos(x)"

eval "(x^2+x)'x+1"
"2*x+2"
```

Version 0.1.5: Define functions in context (previously only values)
Vars and functions can be named with the same name, like in elixir, arity in a name makes it different so:
```
Exun.eval "f*f(y)*f(y,3)", %{"f"=>"3", "f(x)"=>"x^2", "f(a,b)"=>"a^2+a*b+b^2"}
"3*(9+3*y+y^2)*y^2"

Exun.eval " f * f(x)'x * f(y)", %{"f"=>"3", "f(x)"=>"x^2"}
"6*x*y^2"

```


If you are interested in parsing, use 'parse' or 'eval_ast'
```
parse "(a+b)^2/c"
{:divi, {:elev, {:suma, {:vari, "a"}, {:vari, "b"}}, {:numb, 2}}, {:vari, "c"}}

eval_ast "(a+b)^2/c", %{"a"=>"20[m]","b"=>"2[cm]","c"=>"3[s^2]"}
{:unit, {:numb, 133.60013333333333},
 {:divi, {:elev, {:vari, "m"}, {:numb, 2}}, {:elev, {:vari, "s"}, {:numb, 2}}}}
```

This library use an AST built with erlang's yecc parser and transformation in elixir like this:
```
  def mk({:suma, a, @zero}), do: mk(a)
  def mk({:suma, {:numb, n1}, {:numb, n2}}), do: {:numb, n1 + n2}
  def mk({:suma, {:numb, _}, {:unit, _, _}}), do: throw(@invalid_unit_operation)
```
## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `exun` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:exun, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/exun](https://hexdocs.pm/exun).

