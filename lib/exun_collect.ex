defmodule Exun.Collect do
  alias Exun.Eq
  alias Exun.Unit

  @uno {:numb, 1}
  @zero {:numb, 0}
  @muno {:numb, -1}
  @dos {:numb, 2}
  @infinite {:numb, :infinite}

  def mk({{:m, :suma}, lst}) do
    [head | tail] = lst

    tail
    |> Enum.reduce([], fn expr, ac ->
      [cbs(head, expr) | ac]
    end)
  end

  def mk(tree) do
    tree
  end

  @doc """
  Returns commom base for two trees. Used for collect {:m,:sum}
  """
  def cbs({:vari, v}, {:vari, v}) do
    {:ok, {:vari, v}, @uno, @uno}
  end

  def cbs(base={:vari, _v}, {{:m,:mult},products}) do
    products
    |> Enum.reduce([],fn expr,ac ->
      [cbs(base,expr) | ac]
    end)
  end

  def cbs(t1, t2) do
    {:err, "unbased", t1, t2}
  end

    @doc """
  a*(a*b) -> b*a^2
  """
  def scoll({:mult, a, {:mult, a, b}}) do
    scoll({:mult, b, {:elev, a, @dos}})
  end

  def scoll({:mult, {:mult, a, b}, {:mult, a, c}}) do
    scoll({:mult, b, {:mult, c, {:elev, a, @dos}}})
  end

  @doc """
  a*a^b -> a^(b+1)
  """
  def scoll({:mult, a, {:elev, a, b}}) do
    scoll({:elev, a, {:suma, b, @uno}})
  end

  def scoll({:mult, {:mult, a, b}, {:elev, a, c}}) do
    scoll({:mult, b, {:mult, c, {:elev, a, {:suma, c, {:numb, 1}}}}})
  end

  @doc """
  a*b^-1 -> a/b
  """
  def scoll({:mult, a, {:elev, b, {:numb, -1}}}) do
    {:divi, a, b}
  end

  @doc """
  a^b^c -> a^(b*c)
  """
  def scoll({:elev, a, {:elev, b, c}}) do
    scoll({:elev, a, {:mult, b, c}})
  end

  @doc """
  a^b*a^c -> a^(b+c)
  """
  def scoll({:mult, {:elev, a, b}, {:elev, a, c}}) do
    scoll({:elev, a, {:suma, b, c}})
  end

  @doc """
  a^b/a -> a^(b-1)
  """
  def scoll({:divi, {:elev, a, b}, a}) do
    scoll({:elev, a, {:rest, b, @uno}})
  end

  @doc """
  a^b/a^c -> a^(b-c)
  """
  def scoll({:divi, {:elev, a, b}, {:elev, a, c}}) do
    scoll({:elev, a, {:rest, b, c}})
  end

  @doc """
  a*b/a -> b
  """
  def scoll({:divi, {:mult, a, b}, a}) do
    b
  end

  @doc """
  a/b*b -> a
  """
  def scoll({:mult, {:divi, a, b}, b}) do
    a
  end

  @doc """
  a+a -> 2*a
  """
  def scoll({:suma, a, a}) do
    scoll({:mult, {:numb, 2}, a})
  end

  def scoll({:suma, {:mult, a, b}, a}) do
    scoll({:mult, {:suma, b, @uno}, a})
  end

  @doc """
  a*b/a^n -> b*a^(1-n)
  """
  def scoll({:divi, {:mult, a, b}, {:elev, a, c}}) do
    scoll({:mult, b, {:elev, a, {:rest, @uno, c}}})
  end

  @doc """
  Collect tree and simplify maths where
  applicable
  """
  def scoll({op, l, r}) do
    cl = scoll(l)
    cr = scoll(r)
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
          un -> scoll({:unit, {:mult, un_a, {:numb, un_n}}, un_b})
          nu -> scoll({:unit, {:mult, nu_a, {:numb, nu_n}}, nu_b})
          uu -> scoll({:unit, {:mult, a1, a2}, {:mult, b1, b2}})
          cl == @uno -> cr
          cr == @uno -> cl
          cl == @zero or cr == @zero -> @zero
          Eq.eq(cl, cr) -> {:elev, cl, @dos}
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> scoll({op, cl, cr})
        end

      :divi ->
        cond do
          bn -> {:numb, n1 / n2}
          un -> scoll({:unit, {:divi, un_a, {:numb, un_n}}, un_b})
          nu -> scoll({:unit, {:divi, nu_a, {:numb, nu_n}}, nu_b})
          uu -> scoll({:unit, {:divi, a1, a2}, {:divi, b1, b2}})
          cr == @uno -> cl
          cl == @zero -> @zero
          Eq.eq(cr, cl) -> @uno
          cr == @zero -> @infinite
          Eq.eq(l, cl) and Eq.eq(r, cr) -> {op, cl, cr}
          true -> scoll({op, cl, cr})
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
          true -> scoll({op, cl, cr})
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
          true -> scoll({op, cl, cr})
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
          true -> scoll({op, cl, cr})
        end

      end
  end

  def scoll(tree) do
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

end
