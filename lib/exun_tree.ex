defmodule Exun.Tree do
  alias Exun.Unit

  @zero {:numb, 0}
  @uno {:numb, 1}
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

  def headcollect(tree) do
    case tree|>elem(0) do
      :suma -> comm_collect({:suma,tree|>elem(1), tree|>elem(2)})
      :mult -> comm_collect({:mult,tree|>elem(1), tree|>elem(2)})
      _ -> collect(tree)
    end
  end

  def comm_collect(orig={op,a,b}) do
    order1 = collect orig
    if order1 == orig do
      collect {op,b,a}
    else
      order1
    end
  end

  @doc """
  a*b^-1 -> a/b
  """
  def collect({:mult, a, {:elev, b, {:numb, -1}}}) do
    {:divi, a, b}
  end

  def collect({:mult, {:elev, b, {:numb, -1}}, a}) do
    {:divi, a, b}
  end

  @doc """
  a^b^c -> a^(b*c)
  """
  def collect({:elev, a, {:elev, b, c}}) do
    collect({:elev, a, {:mult, b, c}})
  end

  @doc """
  a^b*a -> a^(b+1)
  """
  def collect({:mult, {:elev, a, b}, a}) do
    collect({:elev, a, {:suma, b, @uno}})
  end

  def collect({:mult, a, {:elev, a, b}}) do
    collect({:elev, a, {:suma, b, @uno}})
  end

  @doc """
  a^b*a^c -> a^(b+c)
  """
  def collect({:mult, {:elev, a, b}, {:elev, a, c}}) do
    collect({:elev, a, {:suma, b, c}})
  end

  @doc """
  a^b/a -> a^(b-1)
  """
  def collect({:divi, {:elev, a, b}, a}) do
    collect({:elev, a, {:rest, b, @uno}})
  end

  @doc """
  a^b/a^c -> a^(b-c)
  """
  def collect({:divi, {:elev, a, b}, {:elev, a, c}}) do
    collect({:elev, a, {:rest, b, c}})
  end

  @doc """
  a*(a*b) -> b*a^2
  """
  def collect({:mult, a, {:mult, a, b}}) do
    collect({:mult, b, {:elev, a, @dos}})
  end

  def collect({:mult, {:mult, a, b}, a}) do
    collect({:mult, b, {:elev, a, @dos}})
  end

  def collect({:mult, {:mult, b, a}, a}) do
    collect({:mult, b, {:elev, a, @dos}})
  end

  @doc """
  a*b/a -> b
  """
  def collect({:divi, {:mult, a, b}, a}) do
    b
  end

  @doc """
  a/b*b -> a
  """
  def collect({:mult, {:divi, a, b}, b}) do
    a
  end

  def collect({:mult, b, {:divi, a, b}}) do
    a
  end

  @doc """
  a-a -> 0
  """
  def collect({:rest, a, a}) do
    @zero
  end

  @doc """
  a+a -> 2*a
  """
  def collect({:suma, a, a}) do
    collect {:mult, {:numb, 2}, a}
  end

  def collect({:suma, {:mult, a, b}, a}) do
    collect {:mult,{:suma,b,@uno}, a}
  end

  def collect({:suma, a, {:mult, a, b}}) do
    collect {:mult,{:suma,b,@uno}, a}
  end

  def collect({:suma, {:mult, b, a}, a}) do
    collect {:mult,{:suma,b,@uno}, a}
  end

  def collect({:suma, a, {:mult, b, a}}) do
    collect {:mult,{:suma,b,@uno}, a}
  end

  @doc """
  a*b/a^n -> b*a^(1-n)
  """
  def collect({:divi, {:mult, a, b}, {:elev, a, c}}) do
    collect({:mult, b, {:elev, a, {:rest, @uno, c}}})
  end

  def collect({:divi, {:mult, b, a}, {:elev, a, c}}) do
    collect({:mult, b, {:elev, a, {:rest, @uno, c}}})
  end

  @doc """
  Collect tree and simplify maths where
  applicable
  """
  def collect({op, l, r}) do
    cl = collect(l)
    cr = collect(r)
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
          un -> collect({:unit, {:mult, un_a, {:numb, un_n}}, un_b})
          nu -> collect({:unit, {:mult, nu_a, {:numb, nu_n}}, nu_b})
          uu -> collect({:unit, {:mult, a1, a2}, {:mult, b1, b2}})
          cl == @uno -> cr
          cr == @uno -> cl
          cl == @zero or cr == @zero -> @zero
          cl == cr -> {:elev, cl, @dos}
          l == cl and r == cr -> {op, cl, cr}
          true -> collect({op, cl, cr})
        end

      :divi ->
        cond do
          bn -> {:numb, n1 / n2}
          un -> collect({:unit, {:divi, un_a, {:numb, un_n}}, un_b})
          nu -> collect({:unit, {:divi, nu_a, {:numb, nu_n}}, nu_b})
          uu -> collect({:unit, {:divi, a1, a2}, {:divi, b1, b2}})
          cr == @uno -> cl
          cl == @zero -> @zero
          cr == cl -> @uno
          cr == @zero -> @infinite
          l == cl and r == cr -> {op, cl, cr}
          true -> collect({op, cl, cr})
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

          cl == cr ->
            {:mult, cl, @dos}

          l == cl and r == cr ->
            {op, cl, cr}

          true ->
            collect({op, cl, cr})
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

          cl == cr ->
            @zero

          l == cl and r == cr ->
            {op, cl, cr}

          true ->
            collect({op, cl, cr})
        end

      :elev ->
        cond do
          bn -> {:numb, :math.pow(n1, n2)}
          un -> {:unit, {:elev, un_a, un_n}, {:elev, un_b, un_n}}
          nu or uu -> throw("Units can not be exponents")
          cr == @zero -> @uno
          cl == @zero -> @zero
          cr == @uno -> cl
          l == cl and r == cr -> {op, cl, cr}
          true -> collect({op, cl, cr})
        end
    end
  end

  def collect(tree) do
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
  def expand(tree, context) do
    expand(tree, context, %{})
  end

  def expand(tree, context, deps) do
    newtree = single_expand(tree, context)

    if tree != newtree do
      expand(newtree, context, deps)
    else
      {tree, deps}
    end
  end

  defp single_expand({op, {left, right}}, context) do
    {op, single_expand(left, context), single_expand(right, context)}
  end

  defp single_expand({:vari, var}, context) do
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

  defp single_expand(a, _context) do
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
end
