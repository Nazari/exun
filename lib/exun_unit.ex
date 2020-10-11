defmodule Exun.Unit do
  alias Exun.Collect

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
    "in" => "2.54[cm]",
    "pie" => "30.48[cm]",
    "mi" => "1609.344[m]",
    "yd" => "91.44[cm]",
    "ft" => "0.3048[m]",
    "pc" => "3.08567818585e16[m]",
    "lyr" => "9.46052840488e15[m]",
    "au" => "149597900000[m]",
    "chain" => "20.1168402337[m]",
    "vara" => "83.6[cm]",
    "rd" => "5.02921005842[m]",
    "fath" => "1.82880365761[m]",
    "angstrom" => "1.0e-10[m]",
    "a" => "100[m^2]",
    "acre" => "4046.87260987[m^2]",
    "l" => "100[cm^3]",
    "galUK" => "4.546092[l]",
    "galC" => "4.54609[l]",
    "gal" => "3.785411784[l]",
    "qt" => "0.946352946[l]",
    "pt" => "0.473176473[l]",
    "cu" => "0.2365882365[l]",
    "ozfl" => "2.95735295625e-2[l]",
    "ozUK" => "28.413075[ml]",
    "tbsp" => "14.78676447813[ml]",
    "tsp" => "4.92892159375[ml]",
    "bbl" => "158987.294928[ml]",
    "bu" => "35239.07[ml]",
    "pk" => "8809.7675[ml]",
    "fbm" => "2359.737216[ml]",
    "yr" => "31556925.9747[s]",
    "d" => "86400[s]",
    "h" => "3600[s]",
    "min" => "60[s]",
    "Hz" => "1[1/s]",
    "lb" => "453.59237[g]",
    "arroba" => "25[lb]",
    "oz" => "28.349523125[g]",
    "slug" => "14.5939029372[kg]",
    "lbt" => "373.2417216[g]",
    "ton" => "907.18474[kg]",
    "tonUK" => "1016.0469088[kg]",
    "t" => "1000[kg]",
    "ozt" => "31.1034768[g]",
    "ct" => "0.2[g]",
    "grain" => "0.06479891[g]",
    "u" => "1.6605402e-24[g]",
    "N" => "101.9716[g*m/s^2]",
    "dyn" => "0.00001[kg*m/s^2]",
    "gf" => "0.00980665[N]",
    "kip" => "4448.22161526[N]",
    "lbf" => "4.44822161526[N]",
    "pdl" => "0.1382549544376[N]",
    "J" => "1[kg*m^2/s^2]",
    "erg" => "0.0000001[J]",
    "cal" => "4.1868[J]",
    "Btu" => "1055.05585262[J]",
    "therm" => "105506000[J]",
    "eV" => "1.60217733e-19[J]",
    "W" => "1[kg*m^2/s^3]",
    "hp" => "745.699871582[W]",
    "CV" => "1[hp]",
    "Pa" => "1[kg/m/s^2]",
    "atm" => "101325[Pa]",
    "bar" => "100000[Pa]",
    "psi" => "6894.75729317[Pa]",
    "torr" => "133.322368421[Pa]",
    "mmHg" => "1[torr]",
    "inHg" => "3386.38815789[Pa]",
    "inH2O" => "248.84[Pa]"
  }
  @doc """
  Sum of two units
  """
  def sum(op, {:unit, {:numb, n1}, t1}, {:unit, {:numb, n2}, t2}, pcontext) do
    {res1, exps1} = to_si({1, t1}, pcontext, 1, %{})
    {res2, exps2} = to_si({1, t2}, pcontext, 1, %{})

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> Exun.tostr(t1) <> " and " <> Exun.tostr(t2)}
    else
      val =
        case op do
          :suma -> (n1 * res1 + n2 * res2) / res1
          :resta -> (n1 * res1 - n2 * res2) / res1
          _ -> throw("Unknown op for Exun.Units.sum")
        end

      {:ok, {:unit, {:numb, val}, t1}}
    end
  end

  @doc """
  Convert first unit to the second unit in convert:
  u1 = Exun.parse "3[m]"
  u2 = Exun.parse "1[cm]"
  Exum.Units.convert(u1,u2,%{}) |> Exum.tostr()

  "300[cm]"
  """
  def convert(_u1 = {:unit, {:numb, n1}, t1}, _u2 = {:unit, {:numb, _n2}, t2}, pcontext) do
    {res1, exps1} = to_si({1, t1}, pcontext, 1, %{})
    {res2, exps2} = to_si({1, t2}, pcontext, 1, %{})

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> Exun.tostr(t1) <> " and " <> Exun.tostr(t2)}
    else
      {:ok, {:unit, {:numb, n1 * res1 / res2}, t2}}
    end
  end

  @doc """
  Factorize unit2 from u1, for example
    iex(1)> {u1,d} = Exun.parse "1[km*kg*A/h^2]", %{}
            {u2,d} = Exun.parse "1[N]", %{}
            factorized = Exun.Unit.factorize(u1,u2,%{})
            Exum.Tree.tostr factorized
            "7.566861148315854e-4[N*A]"

  """
  def factorize({:unit, {:numb, n1}, t1}, {:unit, {:numb, _n2}, t2}, pcontext) do
    # Divide t1 by t2, reduce to si and multiply by t2
    {nres, exps} = to_si({n1, {:divi, t1, t2}}, pcontext, 1, %{})
    "#{nres}[#{Exun.tostr(t2)}#{exps_tostr(exps)}]"
  end

  def factorize(e1, e2) do
    factorize(e1 |> Exun.parse(), e2 |> Exun.parse(), %{})
  end

  @doc """
  Convert unit to International System
  args: unit
  """
  def to_si({:unit, {:numb, n}, tree}) do
    to_si({n, tree}, %{}, 1, %{})
  end

  @doc """
  Convert unit to International System
  args: {coef, ast}, parsed_context, current_exponent, exponents
  returns: {newcoef, exponents}
  """
  def to_si({n, {:mult, left, right}}, pcontext, curr_exp, exps) do
    {left_n, exps} = to_si({1, left}, pcontext, curr_exp, exps)
    {right_n, exps} = to_si({1, right}, pcontext, curr_exp, exps)
    {n * left_n * right_n, exps}
  end

  def to_si({n, {:divi, {:numb, 1}, denom}}, pcontext, curr_exp, exps) do
    {nn, exps} = to_si({1, denom}, pcontext, -curr_exp, exps)
    {n / nn, exps}
  end

  def to_si({n, {:divi, left, right}}, pcontext, curr_exp, exps) do
    {left_n, exps} = to_si({1, left}, pcontext, curr_exp, exps)
    {right_n, exps} = to_si({1, right}, pcontext, -curr_exp, exps)
    {n * left_n / right_n, exps}
  end

  def to_si({n, {:elev, left, {:numb, exponent}}}, pcontext, curr_exp, exps) do
    {left_n, exps} = to_si({1, left}, pcontext, curr_exp * exponent, exps)
    {n * :math.pow(left_n, exponent), exps}
  end

  def to_si({n, {:vari, var}}, pcontext, curr_exp, exps) do
    case @fundamental_units[var] do
      nil ->
        {:unit, {:numb, vdef}, tdef} = get_def(var, pcontext)
        to_si({n * vdef, tdef}, pcontext, curr_exp, exps)

      true ->
        {n, Map.put(exps, var, curr_exp + Map.get(exps, var, 0))}
    end
  end

  def to_si({_a, b}, _pcontext, _curr_exp, _exps) do
    throw("Invalid unit definition: " <> Exun.tostr(b))
  end

  @doc """
  Transform a name into a unit, looking at fundamentals, conversions,
  context and prefixes, in that search order.
  """
  def get_def(name, pcontext) do
    [pref | rest] = name |> String.codepoints()
    rest = to_string(rest)

    cond do
      @fundamental_units[name] != nil ->
        {:unit, 1, {:vari, name}}

      @conversions[name] != nil ->
        @conversions[name]
        |> Exun.parse_text()

      pcontext[name] != nil ->
        pcontext[name]

      @prefixes[pref] != nil and byte_size(rest) > 0 ->
        pref_val = @prefixes[pref]
        {:unit, val, tree} = get_def(rest, pcontext)
        {:unit, {:numb, pref_val * val}, tree}

      true ->
        throw("Undefined unit #{name}")
    end
  end

  def exps_tostr(exps) do
    exps
    |> Enum.reduce("", fn {var, exp}, ac ->
      cond do
        exp == 0 ->
          ac

        exp == 1 ->
          "#{ac}*#{var}"

        exp == -1 ->
          "#{ac}/#{var}"

        exp > 0 ->
          "#{ac}*#{var}^#{exp}"

        exp < 0 ->
          "#{ac}/#{var}^#{-exp}"

        true ->
          ac
      end
    end)
  end

  def toSI(u = {:unit, _, _}) do
    {r, e} = to_si(u)

    {:unit, {:numb, r},
     e
     |> Enum.reject(fn {_a, b} -> b == 0 end)
     |> Enum.reduce({:numb,1}, fn {var, exp}, tree ->
       vvar = {:vari, var}
       vexp = {:numb, exp}
       cond do
         tree == {:numb,1} -> Collect.mk({:elev, vvar, vexp})
         exp == 1 -> {:mult, tree, vvar}
         exp == -1 -> {:divi, tree, vvar}
         exp < 0 -> {:divi, tree, {:elev, vvar, vexp}}
         exp > 0 -> {:mult, tree, {:elev, vvar, vexp}}
       end
     end)}
  end
end
