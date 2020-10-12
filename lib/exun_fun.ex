defmodule Exun.Fun do
  import Exun.Collect
  import Exun

  @zero {:numb, 0}
  @uno {:numb, 1}

  @base %{
    "sin(x)" => {&:math.sin/1, "cos(x)"},
    "cos(x)" => {&:math.cos/1, "-sin(x)"}
  }

  @compounds %{
    "tan(x)" => "sin(x)/cos(x)"
  }

  def deriv(txt, x) do
    txt
    |> parse()
    |> coll()
    |> der({:vari,x})
    |> coll()
    #|> IO.inspect(label: "reduced")
    |> tostr()
  end

  def der({:numb, _}, _x),
    do: @zero

  def der({:unit, _uv, _ut}, _x),
    do: @zero

  def der({:vari, var}, {:vari, x}),
    do: if var==x, do: @uno, else: @zero

  def der({:suma, a, b}, x),
    do: {:suma, der(a, x), der(b, x)}

  def der({:rest, a, b}, x),
    do: {:rest, der(a, x), der(b, x)}

  def der({:mult, a, b}, x),
    do: {:suma, {:mult, der(a, x), b}, {:mult, a, der(b, x)}}

  def der({:divi, a, b}, x),
    do: {:divi, {:rest, {:mult, b, der(a, x)}, {:mult, der(b, x), a}}, {:elev, b, {:numb, 2}}}

  def der(y = {:elev, f, g}, x),
    do:
      {:mult,
       y,
       {:suma,
        {:mult, der(g, x), {:fcall, "ln", [f]}},
        {:mult, g, {:divi, der(f, x), f}}}}
end
