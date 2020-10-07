defmodule Exun.Tree do
  alias Exun.Unit
  alias Exun.Eq

  @zero {:numb, 0}
  @uno {:numb, 1}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @infinite {:numb, :infinite}

  @defop %{
    :elev => {100, "^"},
    :mult => {90, "*"},
    :divi => {90, "/"},
    :suma => {50, "+"},
    :rest => {50, "-"},
    :numb => {200, nil},
    :unit => {200, nil},
    :vari => {200, nil}
  }
  @doc """
  Collects expressions, simplify math
  This works for trees left composed on commutative operations so a*b*c is
  represented {:mult,{:mult,a,b},c} and not {:mult,a,{:mult,b,c}}
  Anyway comm_collect take care for commutative ops.
  """
  def reduce(orig = {op, {op, a, b}, c}) when op in [:suma, :mult] do
    ar = reduce(Eq.sort(a))
    br = reduce(Eq.sort(b))
    cr = reduce(Eq.sort(c))

    res1 = comm_collect({op, {op, ar, br}, cr})
    res2 = comm_collect({op, {op, ar, cr}, br})
    res3 = comm_collect({op, {op, br, cr}, ar})

    cond do
      not Eq.eq(orig, res1) -> reduce(res1)
      not Eq.eq(orig, res2) -> reduce(res2)
      not Eq.eq(orig, res3) -> reduce(res3)
      true -> orig
    end
  end

  def reduce(tree) do
    case tree do
      {:suma, l, r} ->
        comm_collect({:suma, reduce(Eq.sort(l)), reduce(Eq.sort(r))})

      {:mult, l, r} ->
        comm_collect({:mult, reduce(Eq.sort(l)), reduce(Eq.sort(r))})

      _ ->
        coll(Eq.sort(tree))
    end
  end

  # propiedad commutativa de + y *, aplicamos collect en un orden y en el otro
  # Lo ideal seria retornar el mas pequeno, retornamos el primero que
  # sea diferente al original
  def comm_collect(orig = {op, a, b}) do
    ar = coll(a)
    br = coll(b)

    result1 = coll({op, ar, br})
    result2 = coll({op, br, ar})

    cond do
      not Eq.eq(result1, orig) ->
        result1

      not Eq.eq(result2, orig) ->
        result2

      true ->
        orig
    end
  end

  @doc """
  a*(a*b) -> b*a^2
  """
  def coll({:mult, a, {:mult, a, b}}) do
    reduce({:mult, b, {:elev, a, @dos}})
  end

  def coll({:mult, {:mult, a, b}, {:mult, a, c}}) do
    reduce({:mult, b, {:mult, c, {:elev, a, @dos}}})
  end

  @doc """
  a*a^b -> a^(b+1)
  """
  def coll({:mult, a, {:elev, a, b}}) do
    reduce({:elev, a, {:suma, b, @uno}})
  end

  def coll({:mult, {:mult, a, b}, {:elev, a, c}}) do
    reduce({:mult, b, {:mult, c, {:elev, a, {:suma, c, {:numb, 1}}}}})
  end

  @doc """
  a*b^-1 -> a/b
  """
  def coll({:mult, a, {:elev, b, {:numb, -1}}}) do
    {:divi, a, b}
  end

  @doc """
  a^b^c -> a^(b*c)
  """
  def coll({:elev, a, {:elev, b, c}}) do
    reduce({:elev, a, {:mult, b, c}})
  end

  @doc """
  a^b*a^c -> a^(b+c)
  """
  def coll({:mult, {:elev, a, b}, {:elev, a, c}}) do
    reduce({:elev, a, {:suma, b, c}})
  end

  @doc """
  a^b/a -> a^(b-1)
  """
  def coll({:divi, {:elev, a, b}, a}) do
    coll({:elev, a, {:rest, b, @uno}})
  end

  @doc """
  a^b/a^c -> a^(b-c)
  """
  def coll({:divi, {:elev, a, b}, {:elev, a, c}}) do
    coll({:elev, a, {:rest, b, c}})
  end

  @doc """
  a*b/a -> b
  """
  def coll({:divi, {:mult, a, b}, a}) do
    b
  end

  @doc """
  a/b*b -> a
  """
  def coll({:mult, {:divi, a, b}, b}) do
    a
  end

  @doc """
  a+a -> 2*a
  """
  def coll({:suma, a, a}) do
    reduce({:mult, {:numb, 2}, a})
  end

  def coll({:suma, {:mult, a, b}, a}) do
    reduce({:mult, {:suma, b, @uno}, a})
  end

  @doc """
  a*b/a^n -> b*a^(1-n)
  """
  def coll({:divi, {:mult, a, b}, {:elev, a, c}}) do
    reduce({:mult, b, {:elev, a, {:rest, @uno, c}}})
  end

  @doc """
  Collect tree and simplify maths where
  applicable
  """
  def coll({op, l, r}) do
    cl = reduce(l)
    cr = reduce(r)
    {bn, n1, n2} = both_numbers(cl, cr)
    {un, un_a, un_b, un_n} = unit_number(cl, cr)
    {nu, nu_n, nu_a, nu_b} = number_unit(cl, cr)
    {uu, a1, b1, a2, b2} = unit_unit(cl, cr)

    case op do
      :unit ->
        {:unit, cl, cr}

      :mult ->
        cond do
          bn -> {:numb, n1 * n2}
          un -> coll({:unit, {:mult, un_a, {:numb, un_n}}, un_b})
          nu -> coll({:unit, {:mult, nu_a, {:numb, nu_n}}, nu_b})
          uu -> coll({:unit, {:mult, a1, a2}, {:mult, b1, b2}})
          cl == @uno -> cr
          cr == @uno -> cl
          cl == @zero or cr == @zero -> @zero
          Eq.eq(cl, cr) -> {:elev, cl, @dos}
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end

      :divi ->
        cond do
          bn -> {:numb, n1 / n2}
          un -> coll({:unit, {:divi, un_a, {:numb, un_n}}, un_b})
          nu -> coll({:unit, {:divi, nu_a, {:numb, nu_n}}, nu_b})
          uu -> coll({:unit, {:divi, a1, a2}, {:divi, b1, b2}})
          cr == @uno -> cl
          cl == @zero -> @zero
          Eq.eq(cr, cl) -> @uno
          cr == @zero -> @infinite
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end

      :elev ->
        cond do
          bn -> {:numb, :math.pow(n1, n2)}
          un -> {:unit, {:elev, un_a, un_n}, {:elev, un_b, un_n}}
          nu or uu -> throw("Units can not be exponents")
          cr == @zero -> @uno
          cl == @zero -> @zero
          cr == @uno -> cl
          cr == @muno -> {:divi, @uno, cl}
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end
      :suma ->
        cond do
          bn -> {:numb, n1 + n2}
          uu ->
            case Unit.sum(:suma, cl, cr, %{}) do
              {:err, msg} -> throw(msg)
              {:ok, result} -> result
            end
          un or nu -> throw("Inconsistent sum of unit and no_unit")
          cr == @zero -> cl
          cl == @zero -> cr
          Eq.eq(cl, cr) -> {:mult, cl, @dos}
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end

      :rest ->
        cond do
          bn -> {:numb, n1 - n2}
          un or nu -> throw("Inconsistent sum of unit and no_unit")
          uu ->
            case Unit.sum(:resta, cl, cr, %{}) do
              {:err, msg} -> throw(msg)
              {:ok, unit} -> unit
            end

          cr == @zero -> cl
          Eq.eq(cl, cr) -> @zero
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end

      end
  end

  def coll(tree) do
    tree
  end

  @doc """
  Both args are units?
  """
  def unit_unit({:unit, a1, b1}, {:unit, a2, b2}) do
    {true, a1, b1, a2, b2}
  end

  def unit_unit(_a, _b) do
    {false, nil, nil, nil, nil}
  end

  @doc """
  Number and unit?
  """
  def number_unit({:numb, n}, {:unit, a, b}) do
    {true, n, a, b}
  end

  def number_unit(_a, _b) do
    {false, nil, nil, nil}
  end

  @doc """
  UNit and number?
  """
  def unit_number({:unit, a, b}, {:numb, n}) do
    {true, a, b, n}
  end

  def unit_number(_a, _b) do
    {false, nil, nil, nil}
  end

  @doc """
  Both numbers?
  """
  def both_numbers({:numb, n1}, {:numb, n2}) do
    {true, n1, n2}
  end

  def both_numbers(_one, _other) do
    {false, 0, 0}
  end

  @doc """
  Expand definitions in context into
  main tree expression until no more
  expansion is posssible
  """
  def replace(tree, pc) do
    newtree = repl(tree, pc)

    if not Eq.eq(tree, newtree) do
      replace(newtree, pc)
    else
      newtree
    end
  end

  def repl({:vari, var}, pc) do
    Map.get(pc, var, {:vari, var})
  end

  def repl({op, l, r}, pc) do
    {op, replace(l, pc), replace(r, pc)}
  end

  def repl(other, _pc) do
    other
  end

  @doc ~S"""
  Translate tree to human readable math expression:
    iex(1)> {tree, deps} = Exun.parse "4*x^(y+1)/z",%{"z"=>"y+1"}
    {{:divi,
    {:mult, {:numb, 4}, {:elev, {:vari, "x"}, {:suma, {:vari, "y"}, {:numb, 1}}}},
    {:vari, "z"}}, %{"z" => {:suma, {:vari, "y"}, {:numb, 1}}}}

  """
  def tostr({:vari, var}) do
    var
  end

  def tostr({:unit, n, tree}) do
    tostr(n) <> "[" <> tostr(tree) <> "]"
  end

  def tostr({:numb, n}) do
    to_string(n)
  end

  def tostr({op, l, r}) do
    {hpri, hstr} = @defop[op]
    {lpri, _} = @defop[l |> elem(0)]
    {rpri, _} = @defop[r |> elem(0)]

    ltxt = tostr(l)
    rtxt = tostr(r)

    cond do
      hpri > lpri and hpri > rpri ->
        "(" <> ltxt <> ")" <> hstr <> "(" <> rtxt <> ")"

      hpri > lpri ->
        "(" <> ltxt <> ")" <> hstr <> rtxt

      hpri > rpri ->
        ltxt <> hstr <> "(" <> rtxt <> ")"

      true ->
        ltxt <> hstr <> rtxt
    end
  end
end
