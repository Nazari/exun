# Exun

Symbolic math library for Elixir, with unit support.
Beta state, please provide feedback, this lib has to be deeply tested .

TODO:
 - Temperature unit conversions
 - Summatory
 - Fractions, to avoid decimal ops
 - Define equations, not only expressions: Isolate variables
 - Add more testing and revise docs
 
DONE:
 + Symbolic math pattern match expressions
 + Derivate
 + Simple integration (pol, trig, ln), miss parts and subst
 + Units (factorize, conversion, operation, user definition)
 + Context definition for vars and funcs
 + Functions and User functions  
 + Partially implemented Multiprocess, make reductions in parallell via Tasks 

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

Derivate and support some functions (trigonometrics, hyperbolics, ln):
Operator ' is derivate, so "f'x" is df(x)/dx; rule "expr'var" means derivate 'expr' for 'var'. 
```
eval "(1+x)^2'x"
"2*(1+x)"

eval "sin(2*x)'x"
"2*cos(x)"

eval "(x^2+x)'x+1"
"2*x+2"
```

Define functions in context
Vars and functions can be named with the same name, like in elixir, arity in a name makes it different so:
```
Exun.eval "f*f(y)*f(y,3)", %{"f"=>"3", "f(x)"=>"x^2", "f(a,b)"=>"a^2+a*b+b^2"}
"3*(9+3*y+y^2)*y^2"

Exun.eval " f * f(x)'x * f(y)", %{"f"=>"3", "f(x)"=>"x^2"}
"6*x*y^2"

```

 Integrate simple expression, not yet implemented Parts or Subst methods. Symbol for integration is $, rule "$expr,var" means integrate 'expr' for 'var'
```
iex(1)> Exun.eval "$3*x^2+2*x+1,x"
"x*(1+x*(1+x))"

iex(5)> Exun.eval "$sin(x),x"     
"-cos(x)"

iex(6)> Exun.eval "$ln(f(x)),x"
"-$x/f(x),x+x*ln(f(x))"
```

Pattern Match expressions in module Pattern:
```
import Exun.Pattern

umatch "u*v'x","x*cos(x)"
Match group ok
  u     => cos(x)
  v     => 0.5*x^2
  v'    => x
Match group ok
  u     => x
  v     => sin(x)
  v'    => cos(x)

umatch "g'x*g^n", "3*x^2*(x^3+1)^2"
Match group ok
  g     = 1+x^3
  n     = 2
  g'x   = 3*x^2

umatch("g(y)+f'x","1+x+y")
Match group ok
  f     => x+0.5*x^2
  f'    => x+1
  g     => y
Match group ok
  f     => x+0.5*x^2
  f'    => 1+x
  g     => y
Match group ok
  f     => 0.5*x^2
  f'    => x
  g     => y+1
...

umatch("f(2*x)","sin(2*x)")
Match group ok
  f     => sin
  x     => x
```

Isolation, Module Exun.Isol can isolate an ast from a tree.
It extracts all instances of ast from the equation, so it can return
more than one result. Ast can be any valid ast, not only a variable ({:vari, name}) 
For example:
```
iex(1)> Exun.Isol.isol (Exun.parse_text "x^2-3=13"), (Exun.parse_text "x")
[ok: {:numb, 4}]

iex(2)> mp1=Exun.Isol.isol (Exun.parse_text "x^2+sin(x)-3=13"), (Exun.parse_text "x")
[
  ok: {:fcall, "asin",
   [{{:m, :suma}, [numb: 16, minus: {:elev, {:vari, "x"}, {:numb, 2}}]}]},
  ok: {:fcall, "exp",
   [
     {{:m, :mult},
      [
        {:numb, 0.5},
        {:fcall, "ln",
         [{{:m, :suma}, [numb: 16, minus: {:fcall, "sin", [vari: "x"]}]}]}
      ]},
     {:numb, 2}
   ]}
]

iex(3)> mp1 |> Enum.map(fn {_,sol} -> Exun.UI.tostr(sol)end)
["asin(16-x^2)", "exp(0.5*ln(16-sin(x)),2)"]

iex(4)> mp1=Exun.Isol.isol (Exun.parse_text "x^2+sin(x)-3=13"), (Exun.parse_text "x^2")
[ok: {{:m, :suma}, [numb: 16, minus: {:fcall, "sin", [vari: "x"]}]}]

iex(5)> mp1 |> Enum.map(fn {_,sol} -> Exun.UI.tostr(sol)end)                           
["16-sin(x)"]

```

I've createt the vector and matrix types in yecc, and a module Exun.Matrix, want to
create basic algebraic (+,-,*,/) for those concepts and may be eigenvalues for nxn symbolic matrices. The elements for
now are symbolic also, so you will be able to write something like the example below. The symbols
for matrix and vector are '{}'.
```
iex(1)> Exun.parse_text "{{x^2,x,1},{sin(x),cos(x),tan(x)},{1,2,3}}"
{{:raw, 3, 3},
 [
   {{:vector, 3}, [{:elev, {:vari, "x"}, {:numb, 2}}, {:vari, "x"}, {:numb, 1}]},
   {{:vector, 3},
    [
      {:fcall, "sin", [vari: "x"]},
      {:fcall, "cos", [vari: "x"]},
      {:fcall, "tan", [vari: "x"]}
    ]},
   {{:vector, 3}, [numb: 1, numb: 2, numb: 3]}
 ]}

iex(2)> Exun.Matrix.uni_m 4
{{:unity, 4, 4}, nil}

iex(3)> Exun.Matrix.pol_m [{:numb,1},{:numb,2},{:numb,3},{:vari,"x"}]
{{:polynom, 3, 3},
 [
   {:elev, {:vari, "x"}, {:numb, -1}},
   {{:m, :mult}, [{:numb, 2}, {:elev, {:vari, "x"}, {:numb, -1}}]},
   {{:m, :mult}, [{:numb, 3}, {:elev, {:vari, "x"}, {:numb, -1}}]}
 ]}
```

Multiprocess. Base measurement for speed will be the brutal expression:
```
iex(5)> :timer.tc(Exun,:eval,["(g(a^b,b^a)/g(b^a,a^b))'a", %{"g(x,y)"=>"(x^y/ln(sinh(y^x))+y^tanh(x)/cos(x*y))'x'y'x"}])
{5327979,
 "(-(-4*(-2*(-a^b^(1+a)*b^a^(1+b)/sinh(b^a^(1+b))*cosh(b^a^(1+b))*ln(b" <> ...}
 ```


If you are interested in parsing, use 'parse' or 'eval_ast'
```
parse "(a+b)^2/c"
{{{:m, :mult},
  [
    {:elev, {:vari, "c"}, {:numb, -1}},
    {:elev, {{:m, :suma}, [vari: "a", vari: "b"]}, {:numb, 2}}
  ]}, %{}}

eval_ast "(a+b)^2/c", %{"a"=>"20[m]","b"=>"2[cm]","c"=>"3[s^2]"}
{:m, :mult},
 [
   {:elev, {:vari, "c"}, {:numb, -1}},
   {:elev, {{:m, :suma}, [vari: "a", vari: "b"]}, {:numb, 2}}
 ]}
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
    {:exun, "~> 0.4.4"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/exun](https://hexdocs.pm/exun).

