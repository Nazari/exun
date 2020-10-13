# Exun

Symbolic math library for Elixir, with unit support.
Alpha state.

TODO:
 - Temperature unit conversions
 - Integration
 - Fractions, to avoid decimal ops
 - User functions definition

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

Call Exun.Unit.help for a list of supported units, you can also add new units via context:
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

Version 0.2.0 can derivate and support some functions (trigonometrics):
Operator ' is derivate, so "f'x" is df(x)/dx
```
eval "(1+x)^2'x"
"2*(1+x)"

eval "sin(2*x)'x"
"2*cos(x)"

eval "(x^2+x)'x+1"
"2*x+2"
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

