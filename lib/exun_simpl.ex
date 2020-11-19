defmodule Exun.Simpl do
  alias Exun.Unit, as: Un
  alias Exun.Eq, as: E
  require Integer

  @zero {:numb, 0, 1}
  @uno {:numb, 1, 1}
  @muno {:numb, -1, 1}
  @dos {:numb, 2, 1}

  @moduledoc """
  Simplify expressions
  """
  @doc """
  Recursively try to simplify expression. Multiple tries are performed.
  For a more agressive simplify, use Exun.Collect.coll
  """
  def mkrec(ast) when is_tuple(ast) do
    nast =
      ast
      |> normalize
      |> mk

    if E.eq(nast, ast),
      do: nast,
      else: nast |> mkrec
  end

  def mkrec(%Exun{ast: ast, pc: pc}) do
    %Exun{ast: mkrec(ast), pc: pc}
  end

  @doc """
  Normalize some structs, mainly {{:m,op},l} so signs can be comparables
  """
  def normalize(ast) do
    case ast do
      {:unit, value, {:numb, 1, 1}} ->
        normalize(value)

      {:unit, value, units} ->
        {:unit, normalize(value), units}

      {{:m, :suma}, [unique]} ->
        normalize(unique)

      {{:m, :mult}, [unique]} ->
        normalize(unique)

      {{:m, :mult}, list} ->
        {newlist, count_negs} =
          Enum.map(list, &normalize/1)
          |> Enum.reduce({[], 0}, fn el, {newlist, count_negs} ->
            if !signof(el) do
              {[chsign(el) | newlist], count_negs + 1}
            else
              {[el | newlist], count_negs}
            end
          end)

        newop = {{:m, :mult}, newlist |> Enum.sort(&E.smm/2)}

        if Integer.is_even(count_negs) do
          newop
        else
          {:minus, newop}
        end

      {{:m, :suma}, list} ->
        {{:m, :suma}, list |> Enum.map(&normalize/1) |> Enum.sort(&E.smm/2)}

      {:minus, {{:m, :suma}, list = [h | _]}} ->
        if(!signof(h)) do
          {{:m, :suma}, Enum.map(list, fn el -> chsign(normalize(el)) end)}
        else
          {:minus, {{:m, :suma}, Enum.map(list, &normalize/1)}}
        end

      {:minus, {:minus, a}} ->
        normalize(a)

      {:minus, a} ->
        {:minus, normalize(a)}

      # if numb is even you can change the sign of base to positive if need
      {:elev, base, num = {:numb, _, _}} ->
        base = normalize(base)

        cond do
          num == @zero ->
            @uno

          is_par(num) and is_gtzero(num) and !signof(base) ->
            {:elev, chsign(base), num}

          !is_par(num) and !is_gtzero(num) and !signof(base) ->
            {:minus, {:elev, chsign(base), num}}

          !signof(base) ->
            {:minus, {:elev, chsign(base), num}}

          true ->
            ast
        end

      {:deriv, f, v} ->
        {:deriv, normalize(f), v}

      {:integ, f, v} ->
        {:integ, normalize(f), v}

      {:fcall, name, list} ->
        {:fcall, name, Enum.map(list, &normalize/1)}

      other ->
        other
    end
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

      {:minus, {:numb, n, d}} ->
        {:numb, -n, d}

      {:minus, a} ->
        {:minus, mk(a)}

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
        Exun.Fun.fcall(name, lst)

      {:deriv, f, v} ->
        Exun.Der.deriv(f, v)

      {:integ, f, v = {:vari, _}} ->
        Exun.Integral.integ({:integ, f, v})

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
            0 -> unity
            # Only one element, replace {{}:m,op},lst} with it
            1 -> List.first(lst)
            # Let's play, try to extract commons from list
            _ -> cfactor({{:m, op}, lst})
          end
        end

      {op, a, b} ->
        {op, mk(a), mk(b)}

      tree ->
        tree
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
                {not sign, sublist ++ newlist}

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
  Reduce literals to one unit or one constant if possible
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
                  number_unit = mkrec(mult(acu_nd, u_nd))
                  tree_unit = mkrec(mult(acu_t, u_t))

                  case tree_unit do
                    {:numb, _, _} ->
                      {ufc.(nd_ac, number_unit), nil, rest}

                    _ ->
                      {nd_ac, {:unit, number_unit, tree_unit}, rest}
                  end

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
      {nd, {:unit, und, tree}} -> [{:unit, mult(nd, und), mk(tree)} | lst]
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

      other ->
        {:minus, other}
    end
    |> normalize()
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
    |> normalize()
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
    |> normalize
  end

  @doc """
  Returns normalized sign of ast
  """
  def signof(ast) do
    case ast do
      {:minus, a} ->
        not signof(a)

      {:numb, a, b} ->
        a / b >= 0

      {:elev, a, b} ->
        case b do
          {:numb, _, _} ->
            if is_par(b) and is_gtzero(b) do
              true
            else
              signof(a)
            end

          _ ->
            signof(a)
        end

      {{:m, _}, _} ->
        true

      {:unit, a, _} ->
        signof(a)

      {:fcall, _, _} ->
        true

      {:deriv, f, _} ->
        signof(f)

      {:integ, f, _} ->
        signof(f)

      {:vari, _} ->
        true
    end
  end

  def is_par(ast) do
    case ast do
      {:numb, n, d} ->
        num = n / d

        if floor(num) == num do
          Integer.is_even(floor(num))
        else
          false
        end

      _ ->
        false
    end
  end

  def is_gtzero({:numb, n, d}) do
    if (n > 0 and d > 0) or (n < 0 and d < 0), do: true, else: false
  end

  @doc """
  Extract common factors from mult
  """
  def cfactor({{:m, op}, lst}) do
    case Enum.reduce(lst, [], fn pivot, nl ->
           remain = lst |> List.delete(pivot)

           nl ++
             Enum.reduce(remain, [], fn opand, subs ->
               case subst(op, pivot, opand) do
                 {:ok, s} -> [{pivot, opand, s} | subs]
                 _ -> subs
               end
             end)
         end) do
      [] ->
        {{:m, op}, lst}

      [{p, o, s} | _] ->
        {{:m, op}, [s | lst |> List.delete(p) |> List.delete(o)]}
        |> normalize()
    end
  end

  def subst(op, pivot, opand) do
    case {op, pivot, opand} do
      {:suma, a, a} ->
        {:ok, mult(@dos, a)}

      {:suma, a, {:minus, a}} ->
        {:ok, @zero}

      {:suma, {:elev, base, e1}, {:elev, base, e2}} ->
        {:ok, mult({:elev, base, e2}, suma(@uno, {:elev, base, rest(e1, e2)}))}

      {:suma, {:elev, base, e1}, {:elev, {:minus, base}, e2}} ->
        {:ok, mult({:elev, base, e2}, suma(@muno, {:elev, base, rest(e1, e2)}))}

      {:suma, base, {:elev, base, exp}} ->
        {:ok, mult(base, suma(@uno, elev(base, rest(exp, @uno))))}

      {:suma, {:minus, base}, {:elev, base, exp}} ->
        {:ok, mult(base, suma(@muno, elev(base, rest(exp, @uno))))}

      {:suma, base, {:minus, {:elev, base, exp}}} ->
        {:ok, mult(base, suma(@uno, {:minus, elev(base, rest(exp, @uno))}))}

      {:suma, {:minus, {{:m, :mult}, p1}}, {:minus, {{:m, :mult}, p2}}} ->
        sumofmults(false, p1, false, p2)

      {:suma, {{:m, :mult}, p1}, {:minus, {{:m, :mult}, p2}}} ->
        sumofmults(true, p1, false, p2)

      {:suma, {{:m, :mult}, p1}, {{:m, :mult}, p2}} ->
        sumofmults(true, p1, true, p2)

      {:suma, a, {{:m, :mult}, l}} ->
        if a in l do
          extract = l |> List.delete(a)
          {:ok, {{:m, :mult}, [a, suma(@uno, {{:m, :mult}, extract})]}}
        else
          {:err, nil}
        end

      {:mult, a, a} ->
        {:ok, elev(a, @dos)}

      {:mult, a, {:minus, a}} ->
        {:ok, {:minus, elev(a, @dos)}}

      {:mult, {:elev, base, e1}, {:elev, base, e2}} ->
        {:ok, {:elev, base, suma(e1, e2)}}

      {:mult, base, {:elev, base, exp}} ->
        {:ok, elev(base, suma(exp, @uno))}

      _ ->
        {:err, nil}
    end
  end

  defp sumofmults(s1, p1, s2, p2) do
    allsubst =
      Enum.reduce(p1, [], fn op1, ac1 ->
        remainl1 = List.delete(p1, op1)

        Enum.reduce(p2, [], fn op2, ac2 ->
          case {op1, op2} do
            {a, a} ->
              remainl2 = List.delete(p2, op2)
              m1 = {{:m, :mult}, remainl1}
              m2 = {{:m, :mult}, remainl2}

              coefs =
                case {s1, s2} do
                  {true, true} -> suma(m1, m2)
                  {true, false} -> rest(m1, m2)
                  {false, false} -> {:minus, suma(m1, m2)}
                end

              [{:ok, mult(a, coefs)} | ac2]

            {a, {:minus, a}} ->
              remainl2 = List.delete(p2, op2)
              m1 = {{:m, :mult}, remainl1}
              m2 = {{:m, :mult}, remainl2}

              coefs =
                case {s1, s2} do
                  {true, true} -> suma(m1, m2)
                  {true, false} -> rest(m1, m2)
                  {false, false} -> {:minus, suma(m1, m2)}
                end

              [{:ok, minus(mult(a, coefs))} | ac2]

            {{:elev, base, e1}, {:elev, base, e2}} ->
              remainl2 = List.delete(p2, op2)

              {e1, remainl1, e2, remainl2} =
                case gt(e2, e1) do
                  {:ok, true} -> {e1, remainl1, e2, remainl2}
                  {:unknown, _} -> {e1, remainl1, e2, remainl2}
                  {:ok, false} -> {e2, remainl2, e1, remainl1}
                end

              aux1 = {{:m, :mult}, remainl1}
              aux2 = {{:m, :mult}, [{:elev, base, rest(e2, e1)} | remainl2]}

              coefs =
                case {s1, s2} do
                  {true, true} -> suma(aux1, aux2)
                  {true, false} -> rest(aux1, aux2)
                  {false, false} -> {:minus, suma(aux1, aux2)}
                end

              [
                {:ok, mult({:elev, base, e1}, coefs)}
                | ac2
              ]

            {base, {:elev, base, exp}} ->
              remainl2 = List.delete(p2, op2)
              aux1 = [{:elev, base, rest(exp, @uno)} | remainl2]

              coefs =
                case {s1, s2} do
                  {true, true} -> {{:m, :suma}, [@uno | aux1]}
                  {true, false} -> rest(@uno, {{:m, :suma}, aux1})
                  {false, false} -> {:minus, {{:m, :suma}, [@uno | aux1]}}
                end

              [{:ok, mult(base, coefs)} | ac2]

            _ ->
              ac2
          end
        end) ++
          ac1
      end)

    case allsubst do
      [] ->
        {:err, nil}

      _ ->
        List.first(allsubst)
    end
  end

  defp gt({:numb, n1, d1}, {:numb, n2, d2}) do
    {:ok, n1 / d1 > n2 / d2}
  end

  defp gt({:elev, a, e1}, {:numb, a, e2}) do
    gt(e1, e2)
  end

  defp gt(_, _) do
    {:unknown, nil}
  end
end
