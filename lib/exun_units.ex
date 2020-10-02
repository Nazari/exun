defmodule Exun.Units do
  alias Exun.Eval

  @prefixes %{
    # Exa
    "E" => 1.0e18,
    # Peta
    "P" => 1.0e15,
    # Tera
    "T" => 1.0e12,
    # Giga
    "G" => 1.0e9,
    # Mega
    "M" => 1.0e6,
    # Kilo
    "k" => 1000,
    # hecto
    "h" => 100,
    # deci
    "d" => 0.1,
    # centi
    "c" => 0.01,
    # mili
    "m" => 0.001,
    # micro
    "u" => 1.0e-6,
    # nano
    "n" => 1.0e-9,
    # pico
    "p" => 1.0e-12,
    # femto
    "f" => 1.0e-15,
    # atto
    "a" => 1.0e-18
  }
  @fundamental_units %{
    # gram
    "g" => true,
    # second
    "s" => true,
    # candela
    "cd" => true,
    # Ampere
    "A" => true,
    # Kelvin
    "K" => true,
    # mol
    "mol" => true,
    # meter
    "m" => true
  }

  @conversions %{
    "pulg" => "2.54[cm]",
    "pie" => "30.48[cm]",
    "mile" => "1609.344[m]",
    "yard" => "91.44[cm]",
    "legua" => "5.57[km]",
    "vara" => "83.6[cm]",
    "oz" => "28.35[g]",
    "lb" => "454[g]",
    "arroba" => "25[lb]",
    "hour" => "60[min]",
    "min" => "60[s]",
    "day" => "24[hour]",
    "galon" => "3.78[l]",
    "l" => "1[dm^3]",
    "N" => "101.9716[g*m/s^2]",
    "slug" => "14.59[kg*m/s^2]",
    "Pa" => "01[N/m^2]",
    "Psi" => "737.07[kg/m^2]"
  }

  def get_def(name, pcontext) do
    <<pref::utf8, rest::binary>> = name

    cond do
      @fundamental_units[name] != nil ->
        {:unit, 1, name}

      @conversions[name] != nil ->
        @conversions[name]
        |> parseunit()

      @prefixes[pref] != nil and byte_size(rest) > 0 ->
        {:unit, val, tree} = get_def(rest, pcontext)
        pref_val = @prefixes[pref]
        {:unit, val * pref_val, tree}

      pcontext[name] != nil ->
        Eval.to_num(pcontext[name], pcontext)
    end
  end

  def conv_si({:unit, {:numb, n}, tree}, pcontext) do
    type = elem(tree,0)
    case type do
      :vari ->
        tree

      :numb ->
        {:unit, {:numb, n}, tree}

      _ ->
        {type, l, r} = tree
        lu = conv_si(l, pcontext)
        ru = conv_si(r, pcontext)
        execop(type, lu, ru)
    end
  end

  def execop(:mult, {:unit, {{:numb, l_n}}, l_tree}, {:unit, {{:numb, r_n}}, r_tree}) do
    {:unit, { {:numb, l_n*r_n},{:mult,{l_tree,r_tree}}}}
  end

  def parseunit(value) do
    with {:ok, tok, _} <- :exun_lex.string(String.to_charlist(value)),
         {:ok, tree} <- :exun_yacc.parse(tok) do
      tree
    end
  end
end
