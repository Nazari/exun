defmodule Exun.Unit do
  alias Exun.Collect
  alias Exun.UI
  import Exun.Math
  @uno {:numb, 1, 1}
  @muno {:numb, -1,1}
  @zero {:numb, 0, 1}

  @moduledoc """
  Handle Units, converts, sum, multiply, transform to SI and factorize
  """
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
    "K" => 1000,
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
    "slug" => "14.5939029372[Kg]",
    "lbt" => "373.2417216[g]",
    "ton" => "907.18474[Kg]",
    "tonUK" => "1016.0469088[Kg]",
    "t" => "1000[Kg]",
    "ozt" => "31.1034768[g]",
    "ct" => "0.2[g]",
    "grain" => "0.06479891[g]",
    "u" => "1.6605402e-24[g]",
    "N" => "101.9716[g*m/s^2]",
    "dyn" => "0.00001[Kg*m/s^2]",
    "gf" => "0.00980665[N]",
    "kip" => "4448.22161526[N]",
    "lbf" => "4.44822161526[N]",
    "pdl" => "0.1382549544376[N]",
    "J" => "1[Kg*m^2/s^2]",
    "erg" => "0.0000001[J]",
    "cal" => "4.1868[J]",
    "Btu" => "1055.05585262[J]",
    "therm" => "105506000[J]",
    "eV" => "1.60217733e-19[J]",
    "W" => "1[Kg*m^2/s^3]",
    "hp" => "745.699871582[W]",
    "CV" => "1[hp]",
    "Pa" => "1[Kg/m/s^2]",
    "atm" => "101325[Pa]",
    "bar" => "100000[Pa]",
    "psi" => "6894.75729317[Pa]",
    "torr" => "133.322368421[Pa]",
    "mmHg" => "1[torr]",
    "inHg" => "3386.38815789[Pa]",
    "inH2O" => "248.84[Pa]"
  }

  @doc """
  Shows help about unit
  Known Units prefixes:
        E = 1.0e18
        G = 1.0e9
        K = 1000
        M = 1.0e6
        P = 1.0e15
        T = 1.0e12
        a = 1.0e-18
        c = 0.01
        d = 0.1
        f = 1.0e-15
        h = 100
        m = 0.001
        n = 1.0e-9
        p = 1.0e-12
        u = 1.0e-6
  Know fundamental units:
        A = true
        K = true
        cd = true
        g = true
        m = true
        mol = true
        s = true
  Know conversions, you can add more via 'context'
        lb = 453.59237[g]
        torr = 133.322368421[Pa]
        vara = 83.6[cm]
        W = 1[Kg*m^2/s^3]
        dyn = 0.00001[Kg*m/s^2]
        ozt = 31.1034768[g]
        pie = 30.48[cm]
        min = 60[s]
        ozUK = 28.413075[ml]
        hp = 745.699871582[W]
        yr = 31556925.9747[s]
        acre = 4046.87260987[m^2]
        cal = 4.1868[J]
        in = 2.54[cm]
        rd = 5.02921005842[m]
        galUK = 4.546092[l]
        J = 1[Kg*m^2/s^2]
        pdl = 0.1382549544376[N]
        Btu = 1055.05585262[J]
        therm = 105506000[J]
        u = 1.6605402e-24[g]
        h = 3600[s]
        qt = 0.946352946[l]
        gf = 0.00980665[N]
        ft = 0.3048[m]
        ozfl = 2.95735295625e-2[l]
        lbf = 4.44822161526[N]
        Pa = 1[Kg/m/s^2]
        cu = 0.2365882365[l]
        bu = 35239.07[ml]
        bar = 100000[Pa]
        l = 100[cm^3]
        ton = 907.18474[Kg]
        tonUK = 1016.0469088[Kg]
        bbl = 158987.294928[ml]
        chain = 20.1168402337[m]
        pt = 0.473176473[l]
        t = 1000[Kg]
        inH2O = 248.84[Pa]
        pk = 8809.7675[ml]
        inHg = 3386.38815789[Pa]
        ct = 0.2[g]
        mmHg = 1[torr]
        N = 101.9716[g*m/s^2]
        oz = 28.349523125[g]
        galC = 4.54609[l]
        pc = 3.08567818585e16[m]
        mi = 1609.344[m]
        eV = 1.60217733e-19[J]
        kip = 4448.22161526[N]
        Hz = 1[1/s]
        yd = 91.44[cm]
        CV = 1[hp]
        slug = 14.5939029372[Kg]
        fath = 1.82880365761[m]
        arroba = 25[lb]
        d = 86400[s]
        gal = 3.785411784[l]
        angstrom = 1.0e-10[m]
        au = 149597900000[m]
        atm = 101325[Pa]
        lyr = 9.46052840488e15[m]
        tbsp = 14.78676447813[ml]
        tsp = 4.92892159375[ml]
        erg = 0.0000001[J]
        a = 100[m^2]
        grain = 0.06479891[g]
        lbt = 373.2417216[g]
        fbm = 2359.737216[ml]
        psi = 6894.75729317[Pa]
  """
  def help do
    IO.puts("Known Units prefixes:")

    @prefixes
    |> Enum.each(fn {k, v} ->
      IO.puts("\t#{k} = #{v}")
    end)

    IO.puts("Know fundamental units:")

    @fundamental_units
    |> Enum.each(fn {k, v} ->
      IO.puts("\t#{k} = #{v}")
    end)

    IO.puts("Know conversions, you can add more via 'context'")

    @conversions
    |> Enum.each(fn {k, v} ->
      IO.puts("\t#{k} = #{v}")
    end)
  end

  @doc """
  Sum or Rest of two units
  """
  def sum({:unit, a = {:numb, _, _}, t1}, {:unit, b = {:numb, _, _}, t2}, pcontext \\ %{}) do
    {res1, exps1} = to_si2({@uno, t1}, pcontext, @uno, %{})
    {res2, exps2} = to_si2({@uno, t2}, pcontext, @uno, %{})

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> UI.tostr(t1) <> " and " <> UI.tostr(t2)}
    else
      val = divi(suma(mult(a, res1), mult(b, res2)), res1)
      {:ok, {:unit, val, t1}}
    end
  end

  @doc """
  Convert first unit to the second unit in convert:
  ```
    iex(3)> Exun.Unit.convert("3[m]","1[cm]",%{}) |> Exum.UI.tostr()
    "300[cm]"
  ```
  """
  def convert_ast({:unit, n = {:numb, _, _}, t1}, {:unit, {:numb, _n2, _d2}, t2}, pcontext \\ %{}) do
    {res1, exps1} = to_si2({@uno, t1}, pcontext, @uno, %{})
    # |> IO.inspect(label: "convert from unit1")

    {res2, exps2} = to_si2({@uno, t2}, pcontext, @uno, %{})
    # |> IO.inspect(label: "convert to unit2")

    if exps1 != exps2 do
      {:err, "Inconsisten units " <> UI.tostr(t1) <> " and " <> UI.tostr(t2)}
    else
      {:ok, {:unit, divi(mult(n, res1), res2), t2}}
    end
  end

  @doc """
  Convert units, for example
  ```
    iex> import Exun.Unit
    iex> "120[Km/h]" |> convert("m/s")
    "33.333333333333336[m/s]"
  ```
  """

  def convert(tu1, tu2, pcontext \\ %{}) when is_binary(tu1) and is_binary(tu2) do
    {u1, _} = Exun.parse(tu1)
    {u2, _} = Exun.parse("1[" <> tu2 <> "]")

    case convert_ast(u1, u2, pcontext) do
      {:err, msg} -> throw(msg)
      {:ok, res} -> UI.tostr(res)
    end
  end

  @doc """
  Factorize unit u2 from u1, for example
  ```
    iex(1)> {u1,d} = Exun.parse "1[km*Kg*A/h^2]", %{}
            {u2,d} = Exun.parse "1[N]", %{}
            factorized = Exun.Unit.factorize(u1,u2,%{})
            Exum.Tree.tostr factorized
            "7.566861148315854e-4[N*A]"
  ```
  """
  def factorize_ast({:unit, n = {:numb, _, _}, t1}, {:unit, {:numb, _n2, _d2}, t2}, pcontext) do
    # Divide t1 by t2, reduce to si and multiply by t2
    {nres, exps} = to_si2({n, divi(t1, t2)}, pcontext, @uno, %{})
    "#{UI.tostr(nres)}[#{UI.tostr(t2)}#{exps_tostr(exps)}]"
  end

  @doc """
  Factorize units, as text:
  ```
  factorize("2[N]","[Kg]")
  ```
  """
  def factorize(e1, e2, pcontext \\ %{}) do
    {u1, _} = Exun.parse(e1, pcontext)
    {u2, _} = Exun.parse("1" <> e2, pcontext)
    factorize_ast(u1, u2, pcontext)
  end

  defp to_si2({:unit, n = {:numb, _, _}, tree}, pcontext \\ %{}) do
    to_si2({n, tree}, pcontext, @uno, %{})
  end

  defp to_si2({nn, {{:m, :mult}, lst}}, pcontext, curre, exps) do
    Enum.reduce(lst, {nn, exps}, fn opand, {acu_nn, acu_e} ->
      {new_n, ne} = to_si2({@uno, opand}, pcontext, curre, acu_e)
      {mult(acu_nn, new_n), ne}
    end)
  end

  defp to_si2({nn, {:elev, left, nnexp = {:numb, _, _}}}, pcontext, curre, exps) do
    {left_n, exps} = to_si2({@uno, left}, pcontext, mult(curre, nnexp), exps)
    {mult(nn, elev(left_n, nnexp)), exps}
  end

  defp to_si2({n, {:vari, var}}, pcontext, curre, exps) do
    case @fundamental_units[var] do
      nil ->
        {:unit, vdef = {:numb, _, _}, tdef} = get_def(var, pcontext)
        to_si2({mult(n, vdef), tdef}, pcontext, curre, exps)

      true ->
        x1 = Map.get(exps, var, @zero)
        x2 = suma(curre, x1)
        {n, Map.put(exps, var, x2)}
    end
  end

  defp to_si2({a, b}, _pcontext, _curr_exp, _exps) do
    # IO.inspect({:unit, {:numb, a}, b}, label: "For unit:")
    throw("Invalid unit definition: " <> UI.tostr({:unit, {:numb, a, 1}, b}))
  end

  defp get_def(name, pcontext) do
    [pref | rest] = name |> String.codepoints()
    rest = to_string(rest)

    cond do
      @fundamental_units[name] != nil ->
        {:unit, @uno, {:vari, name}}

      @conversions[name] != nil ->
        @conversions[name]
        |> Exun.parse_text()

      pcontext[name] != nil ->
        pcontext[name]

      @prefixes[pref] != nil and byte_size(rest) > 0 ->
        pref_val = {:numb, @prefixes[pref], 1}
        {:unit, val, tree} = get_def(rest, pcontext)
        # IO.inspect(pref_val,label: "pref_val")
        # IO.inspect(val,label: "val")
        {:unit, mult(pref_val, val), tree}

      true ->
        throw("Undefined unit #{name}")
    end
  end

  defp exps_tostr(exps) do
    exps
    |> Enum.reduce("", fn {var, exp}, ac ->
      cond do
        exp == @zero -> ac
        exp == @uno -> "#{ac}*#{var}"
        exp == @muno -> "#{ac}/#{var}"
        signof(exp)==:neg -> "#{ac}/#{var}^#{UI.tostr(chsign(exp))}"
        true -> "#{ac}*#{var}^#{UI.tostr(exp)}"
      end
    end)
  end

  @doc """
  Converts unit to International System
  """
  def toSI({:unit, uv, ut}) do
    {r, e} = to_si2({:unit, Collect.coll(uv), Collect.coll(ut)})

    {:unit, r,
     Collect.coll(
       e
       |> Enum.reject(fn {_a, b} -> b == 0 end)
       |> Enum.reduce(@uno, fn {var, expon}, tree ->
         mult(tree, {:elev, {:vari, var}, expon})
       end)
     )}
  end
end
