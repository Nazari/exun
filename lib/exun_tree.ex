defmodule Exun.Tree do
  alias Exun.Unit

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

  def reduce(orig = {op, left = {op, a, b}, right = c}) when op in [:suma, :mult] do
    res1 = comm_collect({op, left, right})
    res2 = comm_collect({op, {op, a, c}, b})
    res3 = comm_collect({op, {op, b, c}, a})

    cond do
      not eq(orig, res1) -> reduce(res1)
      not eq(orig, res2) -> reduce(res2)
      not eq(orig, res3) -> reduce(res3)
      true -> orig
    end

  end

  def reduce(tree) do
    case tree do
      {:suma,l,r} ->
        comm_collect({:suma,l,r})

      {:mult,l,r} ->
        comm_collect({:mult,l,r})

      _ ->
        coll(tree)
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
      not eq(result1, orig) ->
        result1

      not eq(result2, orig) ->
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
  a-a -> 0
  """
  def coll({:rest, a, a}) do
    @zero
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
          eq(cl, cr) -> {:elev, cl, @dos}
          eq(l, cl) and eq(r, cr) -> {op, cl, cr}
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
          eq(cr, cl) -> @uno
          cr == @zero -> @infinite
          eq(l, cl) and eq(r, cr) -> {op, cl, cr}
          true -> coll({op, cl, cr})
        end

      :suma ->
        cond do
          bn ->
            {:numb, n1 + n2}

          uu ->
            case Unit.sum(:suma, cl, cr, %{}) do
              {:err, msg} -> throw(msg)
              {:ok, result} -> result
            end

          un or nu ->
            throw("Inconsistent sum of unit and no_unit")

          cr == @zero ->
            cl

          cl == @zero ->
            cr

          eq(cl, cr) ->
            {:mult, cl, @dos}

          eq(l, cl) and eq(r, cr) ->
            {op, cl, cr}

          true ->
            coll({op, cl, cr})
        end

      :rest ->
        cond do
          bn ->
            {:numb, n1 - n2}

          un or nu ->
            throw("Inconsistent sum of unit and no_unit")

          uu ->
            case Unit.sum(:resta, cl, cr, %{}) do
              {:err, msg} -> throw(msg)
              {:ok, unit} -> unit
            end

          cr == @zero ->
            cl

          eq(cl, cr) ->
            @zero

          eq(l, cl) and eq(r, cr) ->
            {op, cl, cr}

          true ->
            coll({op, cl, cr})
        end

      :elev ->
        cond do
          bn -> {:numb, :math.pow(n1, n2)}
          un -> {:unit, {:elev, un_a, un_n}, {:elev, un_b, un_n}}
          nu or uu -> throw("Units can not be exponents")
          cr == @zero -> @uno
          cl == @zero -> @zero
          cr == @uno -> cl
          cr == @muno -> {:divi,@uno ,cl}
          eq(l, cl) and eq(r, cr) -> {op, cl, cr}
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
  def replace(tree, context) do
    replace(tree, context, %{})
  end

  def replace(tree, context, deps) do
    newtree = single_replace(tree, context)

    if tree != newtree do
      replace(newtree, context, deps)
    else
      {tree, deps}
    end
  end

  defp single_replace({op, {left, right}}, context) do
    {op, single_replace(left, context), single_replace(right, context)}
  end

  defp single_replace({:vari, var}, context) do
    in_context = Map.get(context, var)

    if in_context != nil do
      with {:ok, toks, _} <- :exun_lex.string(in_context |> to_string() |> String.to_charlist()),
           {:ok, tree} <- :exun_yacc.parse(toks) do
        tree
      end
    else
      {:vari, var}
    end
  end

  defp single_replace(a, _context) do
    a
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

  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    norm(t1) == norm(t2)
  end

  def norm({op, {op, a, b}, {op, c, d}}) when op in [:suma, :mult] do
    {{:m, op}, [norm(a), norm(b), norm(c), norm(d)] |> Enum.sort()}
  end

  def norm({op, c, {op, a, b}}) when op in [:suma, :mult] do
    {{:m, op}, [norm(a), norm(b), norm(c)] |> Enum.sort()}
  end

  def norm({op, {op, a, b}, c}) when op in [:suma, :mult] do
    {{:m, op}, [norm(a), norm(b), norm(c)] |> Enum.sort()}
  end

  def norm({op, a, b}) when op in [:suma, :mult] do
    {{:m, op}, [norm(a), norm(b)] |> Enum.sort()}
  end

  def norm({op,a,b}) do
    {op, norm(a), norm(b)}
  end

  def norm(other) do
    other
  end
end
