# Exun

Symbolic math library for Elixir, with unit support.
TODO:
 - Derivate, Integration

run "iex -S mix" inside exun dir and type:

import Exun
import Exun.Unit
eval "(a+1)*(a+1)/(a+1)^3"

-->
"1/(a+1)"

eval "1[m]+1[cm]"

-->
"1.01[m]"

factorize "1[A*kg*m/s^2]","1[N]"

-->
"9.806652048217348[N*A]"

"120[Km/h]" |> convert("m/s")
"33.3333333333[m/s]"

You can put context also:

Exun.eval "(a+1)^2/b", %{"b"=>"a+1"}

-->
"a+1"

eval "(a+b)^2/c", %{"a"=>"20[m]","b"=>"2[cm]","c"=>"3[s^2]"}
"0.3333333333"

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

