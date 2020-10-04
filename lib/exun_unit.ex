defmodule Exun.Unit do
  alias Exun.Tree

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
    "l" => "100[cm^3]",
    "N" => "101.9716[g*m/s^2]",
    "slug" => "14.59[kg*m/s^2]",
    "Pa" => "1[N/m^2]",
    "Psi" => "737.07[kg/m^2]"
  }
  @doc """
  Sum of two units
  """
  def sum(op, _u1 = {:unit, {:numb, n1}, t1}, _u2 = {:unit, {:numb, n2}, t2}, pcontext) do
    {res1, exps1} = to_si({1, t1}, pcontext, 1, %{})
    {res2, exps2} = to_si({1, t2}, pcontext, 1, %{})

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> Tree.tostr(t1) <> " and " <> Tree.tostr(t2)}
    else
      val =
        case op do
          :suma -> res1 * n1 + res2 * n2
          :resta -> res1 * n1 - res2 * n2
          _ -> throw("Unknown op for Exun.Units.sum")
        end

      {:ok, {:unit, {:numb, val}, t1}}
    end
  end

  @doc """
  Convert first unit to the second unit in convert:
  u1 = Exun.parse "3[m]"
  u2 = Exun.parse "1[cm]"
  Exum.Units.convert(u1,u2,%{}) |> Exum.Tree.tostr() ===> "300[cm]"
  """
  def convert(_u1 = {:unit, {:numb, n1}, t1}, _u2 = {:unit, {:numb, _n2}, t2}, pcontext) do
    {res1, exps1} = to_si({1, t1}, pcontext, 1, %{})
    {res2, exps2} = to_si({1, t2}, pcontext, 1, %{})

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> Tree.tostr(t1) <> " and " <> Tree.tostr(t2)}
    else
      {:ok, {:unit, {:numb, n1 * res1 / res2}, t2}}
    end
  end

  @doc """
  Factorize unit2 from u1, for example
    iex(1)> {u1,d} = Exun.parse "1[km*kg*A/hour^2]", %{}
            {u2,d} = Exun.parse "1[N]"
            factorized = Exun.Unit.factorize(u1,u2,%{})

  Exum.Tree.tostr factorized ====> "7.716049e-5[N*A]

  """
  def factorize({:unit, {:numb, n1}, t1}, {:unit, {:numb, _n2}, t2}, pcontext) do
    # Divide t1 by t2, reduce to si and multiply by t2
    {nres, exps} = to_si({n1, {:divi, t1, t2}}, pcontext, 1, %{})
    "#{nres}[#{Exun.Tree.tostr(t2)}#{exps_tostr(exps)}]"
  end

  @doc """
  Convert unit to International System
  """
  def to_si({:unit, {:numb, n}, tree}) do
    to_si {n,tree}, %{}, 1, %{}
  end

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
    {n * left_n, exps}
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
    throw("Invalid unit definition: " <> Exun.Tree.tostr(b))
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
        |> parseunit()

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

  @doc """
  Parse a unit:
  "1[m]" -> {:unit, {:numb,1}, {:vari, "m"}}
  """
  def parseunit(value) do
    with {:ok, tok, _} <- :exun_lex.string(String.to_charlist(value)),
         {:ok, tree} <- :exun_yacc.parse(tok) do
      tree
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
end