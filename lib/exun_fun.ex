defmodule Exun.Fun do
  @moduledoc """
  Function management, not complete.
  @base and  @compound will be the definitions of external functions.
  """
  import Exun.Collect
  import Exun

  @zero {:numb, 0}
  @uno {:numb, 1}

  @base %{
    "sin" => {&:math.sin/1, "x'*cos"},
    "cos" => {&:math.cos/1, "-x'*sin"},
    "ln"  => {&:math.log/1, "x'/x"}
  }

  @compounds %{
    "tan" => "sin/cos"
  }

  def fcall(name,args) do
    cond do
      (b=@base[name]) != nil ->
        {:bfunc, b}
      (c=@compounds[name]) != nil ->
        {:cfunc, c}
    end
  end
  @doc """
  Derive function txt for variable x, return string
  """
  def deriv(txt, x) do
    txt
    |> parse()
    |> coll()
    |> der({:vari, x})
    |> coll()
    # |> IO.inspect(label: "reduced")
    |> tostr()
  end

  defp der({:numb, _}, _x),
    do: @zero

  defp der({:unit, _uv, _ut}, _x),
    do: @zero

  defp der({:vari, var}, {:vari, x}),
    do: if(var == x, do: @uno, else: @zero)

  defp der({:suma, a, b}, x),
    do: {:suma, der(a, x), der(b, x)}

  defp der({:rest, a, b}, x),
    do: {:rest, der(a, x), der(b, x)}

  defp der({:mult, a, b}, x),
    do: {:suma, {:mult, der(a, x), b}, {:mult, a, der(b, x)}}

  defp der({:divi, a, b}, x),
    do: {:divi, {:rest, {:mult, b, der(a, x)}, {:mult, der(b, x), a}}, {:elev, b, {:numb, 2}}}

  defp der(y = {:elev, f, g}, x),
    do:
      {:mult, y,
       {:suma, {:mult, der(g, x), {:fcall, "ln", [f]}}, {:mult, g, {:divi, der(f, x), f}}}}
end
