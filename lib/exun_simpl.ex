defmodule Exun.Simpl do
  alias Exun.Collect, as: C
  alias Exun.Unit, as: Un
  alias Exun.Eq, as: E

  @zero {:numb, 0, 1}
  @uno {:numb, 1, 1}
  @muno {:numb, -1, 1}
  @moduledoc """
  Simplify expressions
  """
  @doc """
  Recursively try to simplify expression. Multiple tries are performed.
  For a more agressive simplify, use Exun.Collect.coll
  """
  def mkrec(ast) when is_tuple(ast) do
    nast = mk(ast)

    if E.eq(nast, ast),
      do: nast,
      else: nast |> mkrec
  end

  def mkrec(%Exun{ast: ast, pc: pc}) do
    %Exun{ast: mkrec(ast), pc: pc}
  end

  # simplify
  def mk(ast) do
    case ast do
      {:numb, n, d} ->
        mknum(n, d)

      {:minus, {:minus, a}} ->
        mk(a)

      {:minus, @zero} ->
        @zero

      {:minus, a} ->
        chsign(mk(a))

      {:unit, val, @uno} ->
        mk(val)

      {:unit, val, ut} ->
        Un.toSI({:unit, mk(val), mk(ut)})

      {:elev, _, @zero} ->
        @uno

      {:elev, a, @uno} ->
        mk(a)

      {:elev, @uno, _} ->
        @uno

      {:elev, {:numb, base, d1}, {:numb, -1, 1}} ->
        {:numb, d1, base}

      {:elev, {:numb, base, d1}, {:numb, exp, d2}} ->
        {:numb, :math.pow(base, exp / d2), :math.pow(d1, exp / d2)}

      {:elev, {:elev, base, e1}, e2} ->
        {:elev, mk(base), mk(mult(e1, e2))}

      {:elev, {:unit, uv, ut}, expon} ->
        {:unit, mk({:elev, uv, expon}), mk({:elev, ut, expon})}

      {:fcall, name, lst} ->
        args = Enum.map(lst, &C.coll/1)
        Exun.Fun.fcall(name, args)

      {:deriv, f, v} ->
        Exun.Der.deriv({:deriv, mk(f), v})

      {:integ, f, v = {:vari, _}} ->
        Exun.Integral.integ({:integ, mk(f), v})

      {{:m, op}, lst} ->
        # Simplify each component of the list
        lst = Enum.map(lst, &mkrec(&1))

        # Promote sublist, so if ther is a element in lst of class {:m,op}
        # include sublist in main list
        lst = promote_sublist(op, lst)

        # Collect numbers and units and simplify
        lst = collect_literals({{:m, op}, lst})

        # Remove zeroes or ones, 0+any=any, 1*any=any and may be the nil
        # introduced by the last command
        unity = if op == :suma, do: @zero, else: @uno
        lst = Enum.reject(lst, &(&1 == unity or &1 == nil))
        # |> IO.inspect(label: "post literals")

        # if a multiple mult {:m,:mult} check if zero is a component
        if op == :mult and @zero in lst do
          @zero
        else
          case length(lst) do
            # No more elements in list, return unity
            0 ->
              unity

            # Only one element, replace {{}:m,op},lst} with it
            1 ->
              List.first(lst)

            # Let's play, try to extract commons from list
            _ ->
              isolate({{:m, op}, lst})
          end
        end

      {op, a, b} ->
        {op, mk(a), mk(b)}

      tree ->
        tree
    end
  end

  def isolate({{:m, op}, list}) do
    best_match =
      Enum.map(list, fn pivot ->
        coefs =
          Enum.map(list, fn ast ->
            case {op, pivot, ast} do
              {op, a, a} when op in [:suma, :mult] ->
                {:ok, pivot, @uno}

              {op, {:minus, a}, a} when op in [:suma, :mult] ->
                {:ok, pivot, @muno}

              {op, {:elev, a, e1}, {:elev, a, e2}} ->
                case op do
                  :suma -> {:err, nil, ast}
                  :mult -> {:ok, pivot, mk(divi(e2, e1))}
                end

              {op, a, {:elev, a, b}} ->
                case op do
                  :suma -> {:ok, pivot, mk({:elev, a, mk(rest(b, @uno))})}
                  :mult -> {:ok, pivot, b}
                end

              {:suma, a, {{:m, :mult}, lst}} ->
                if a in lst,
                  do: {:ok, pivot, {{:m, :mult}, lst |> List.delete(a)}},
                  else: {:err, nil, ast}

              _ ->
                {:err, nil, ast}
            end
          end)

        # Count matches for pivot
        matches = Enum.count(coefs, fn {res, _, _} -> res == :ok end)
        {matches, coefs}
      end)
      |> Enum.reject(fn {matches, _} -> matches < 2 end)
      |> Enum.sort(fn {e1, _}, {e2, _} -> e1 < e2 end)
      |> List.first()

    case best_match do
      {_, coefs} ->
        {pivot, iso, notiso} =
          Enum.reduce(coefs, {nil, [], []}, fn item, {piv, iso, noiso} ->
            case item do
              {:ok, pivot, coef} ->
                {pivot, [coef | iso], noiso}

              {:err, _, ast} ->
                {piv, iso, [ast | noiso]}
            end
          end)

        sum_coefs = mk({{:m, :suma}, iso})

        case {op, length(iso), length(notiso)} do
          {_, 0, 1} ->
            List.first(notiso)

          {:suma, 0, 0} ->
            @zero

          {:suma, 1, 0} ->
            {{:m, :mult}, [pivot, List.first(iso)]}

          {:suma, 0, 1} ->
            List.first(notiso)

          {:suma, 1, 1} ->
            {{:m, :suma}, [{{:m, :mult}, [pivot, List.first(iso)]}, List.first(notiso)]}

          {:suma, _, 0} ->
            {{:m, :mult}, [pivot, sum_coefs]}

          {:suma, 0, _} ->
            {{:m, :suma}, notiso}

          {:suma, _, _} ->
            {{:m, :suma}, [{{:m, :mult}, [pivot, sum_coefs]} | notiso]}

          {:mult, 0, 0} ->
            @uno

          {:mult, 1, 0} ->
            {:elev, pivot, List.first(iso)}

          {:mult, 0, 1} ->
            List.first(notiso)

          {:mult, 1, 1} ->
            {{:m, :mult}, [{:elev, pivot, List.first(iso)}, List.first(notiso)]}

          {:mult, _, 0} ->
            {:elev, pivot, sum_coefs}

          {:mult, 0, _} ->
            {{:m, :mult}, notiso}

          {:mult, _, _} ->
            {{:m, :mult}, [{:elev, pivot, sum_coefs} | notiso]}
        end
        |> mk()

      nil ->
        {{:m, op}, list}
    end
  end

  defp promote_sublist(op, list) when is_list(list) do
    case op do
      :suma ->
        Enum.reduce(list, [], fn elem, newlist ->
          case elem do
            {{:m, :suma}, sublist} ->
              sublist ++ newlist

            {:minus, {{:m, :suma}, sublist}} ->
              Enum.map(sublist, &chsign/1) ++ newlist

            other ->
              [other | newlist]
          end
        end)

      :mult ->
        {sign, nlist} =
          Enum.reduce(list, {true, []}, fn elem, {sign, newlist} ->
            case elem do
              {{:m, :mult}, sublist} ->
                {sign, sublist ++ newlist}

              {:minus, {{:m, :mult}, sublist}} ->
                {if(sign, do: false, else: true), sublist ++ newlist}

              other ->
                {sign, [other | newlist]}
            end
          end)

        if sign do
          nlist
        else
          List.replace_at(nlist, 0, chsign(List.first(nlist)))
        end
    end
    |> Enum.sort(&Exun.Eq.smm/2)
  end

  @doc """
  Reduce literals to une unit or one constant if possible
  """
  def collect_literals({{:m, op}, lst}) do
    unity = if op == :suma, do: @zero, else: @uno
    ufc = if op == :suma, do: &suma/2, else: &mult/2

    {n, u, lst} =
      Enum.reduce(lst, {unity, nil, []}, fn el, {nd_ac = {:numb, _, _}, u_ac, rest} ->
        case el do
          nd = {:numb, _, _} ->
            {ufc.(nd_ac, nd), u_ac, rest}

          u = {:unit, u_nd = {:numb, _, _}, u_t} ->
            if u_ac == nil do
              {nd_ac, u, rest}
            else
              {:unit, acu_nd, acu_t} = u_ac

              case op do
                :mult ->
                  sou = {:unit, mult(acu_nd, u_nd), mult(acu_t, u_t)}
                  {nd_ac, sou, rest}

                :suma ->
                  sou =
                    case Exun.Unit.sum(u_ac, u) do
                      {:err, msg} -> throw(msg)
                      {:ok, unit} -> unit
                    end

                  {nd_ac, sou, rest}
              end
            end

          other ->
            {nd_ac, u_ac, [other | rest]}
        end
      end)

    case {n, u} do
      {^unity, nil} -> lst
      {^unity, unit} -> [unit | lst]
      {number, nil} -> [number | lst]
      {nd, {:unit, und, tree}} -> [{:unit, mult(nd, und), tree} | lst]
    end

    # |> IO.inspect(label: "final unit,number,lst")
  end

  @doc """
  Change sign of AST
  """
  def chsign(ast) do
    case ast do
      {:minus, a} ->
        a

      {:unit, a, b} ->
        {:unit, chsign(a), b}

      {:numb, n, d} ->
        {:numb, -n, d}

      {{:m, :suma}, list} ->
        {{:m, :suma}, Enum.map(list, &chsign(&1))}

      {{:m, :mult}, list} ->
        # sign true is positive, false is negative
        {res, nl} =
          Enum.reduce(list, {true, []}, fn opand, {sign, nl} ->
            case opand do
              {:numb, -1, 1} ->
                {not sign, nl}

              {:minus, minusop} ->
                {not sign, [minusop | nl]}

              _ ->
                {sign, [opand | nl]}
            end
          end)

        nl = nl |> Enum.reverse()

        if res do
          {:minus, {{:m, :mult}, nl}}
        else
          {{:m, :mult}, nl}
        end

      other ->
        {:minus, other}
    end
  end

  @doc """
  Change power sign of AST (expon * -1 or 1/tree)
  """
  def chpow(ast) do
    case ast do
      {:elev, algo, @muno} ->
        algo

      {:elev, a, b} ->
        {:elev, a, chsign(b)}

      {:unit, a, b} ->
        {:unit, chpow(a), chpow(b)}

      {:numb, n, d} ->
        {:numb, d, n}

      {{:m, :mult}, lst} ->
        {{:m, :mult}, Enum.map(lst, &chpow(&1))}

      other ->
        {:elev, other, @muno}
    end
  end

  @doc """
  Try to keep at least 3 decimals of precission
  """
  def mknum(n, d) do
    # Preserve sign on numerator, if denominator<0 change both
    # signs
    {n, d} =
      if d < 0 do
        {-n, -d}
      else
        {n, d}
      end

    # If are integers, substirute floats
    f_n = floor(n)
    f_d = floor(d)

    cond do
      f_n == n and f_d == d ->
        mcd = Integer.gcd(f_n, f_d)
        {:numb, floor(n / mcd), floor(d / mcd)}

      f_d == d ->
        {:numb, n, f_d}

      f_n == n ->
        {:numb, f_n, d}

      true ->
        {:numb, n, d}
    end
  end

  @doc """
  For convenience, creates ast {{:m,:mult},[a,b]}
  """
  def mult({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * n2, d1 * d2)
  def mult(a, b), do: mcompose(:mult, a, b)

  @doc """
  For convenience, creates ast {{:m,:mult},[a,b^-1]}
  """
  def divi({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2, d1 * n2)
  def divi(a, b), do: mult(a, chpow(b))

  @doc """
  For convenience, creates ast {{:m,:suma},[a,b]}
  """
  def suma({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2 + n2 * d1, d1 * d2)
  def suma(a, b), do: mcompose(:suma, a, b)

  @doc """
  For convenience, creates ast {{:m,:mult},[a,-b]}
  """
  def rest({:numb, n1, d1}, {:numb, n2, d2}), do: mknum(n1 * d2 - n2 * d1, d1 * d2)
  def rest(a, b), do: suma(a, chsign(b))

  @doc """
  For convenience, creates ast {:elev,a,b}
  """
  def elev({:numb, n1, d1}, {:numb, n2, d2}),
    do: {:numb, :math.pow(n1, n2 / d2), :math.pow(d1, n2 / d2)}

  def elev(a, b), do: {:elev, a, b}

  def minus(a), do: {:minus, a}

  def ln(a), do: {:fcall, "ln", [a]}
  def exp(a, b), do: {:fcall, "exp", [a, b]}

  defp mcompose(op, a, b) do
    C.coll(
      case {a, b} do
        {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
          {{:m, op}, l1 ++ l2}

        {{{:m, ^op}, l1}, _} ->
          {{:m, op}, [b | l1]}

        {_, {{:m, ^op}, l2}} ->
          {{:m, op}, [a | l2]}

        _ ->
          {{:m, op}, [a, b]}
      end
    )
  end
end
