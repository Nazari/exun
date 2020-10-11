# Exun

Symbolic math library for Elixir, with unit support.

run "iex -S mix" inside exun dir and type:

Exun.eval "(a+1)*(a+1)/(a+1)^3"

-->
"1/(a+1)"

Exun.eval "1[m]+1[cm]"

-->
"1.01[m]"

Exun.Unit.factorize "1[A*kg*m/s^2]","1[N]"

-->
"9.806652048217348[N*A]"

You can put context also:

Exun.eval "(a+1)^2/b", %{"b"=>"a+1"}

-->
"a+1"

Exun.eval "(a+1)^2/b", %{"b"=>"a+1","a"=>"2"}
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

