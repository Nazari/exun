defmodule Exun.Fun do

  @moduledoc """
  Function management.
  @base and  @compound are the definitions of external functions.
  """

  @doc """
  base is a map that holds functions names and a tupla
   For example:
  ```
  "ln(F)" => {&:math.log/1, "F'x/F", "x*ln(F)-$(x/F),x"}
  ```
  - ln(F) : means function name is 'ln' and can receive Functions as argument
  - &:math.log/1 : Is the Elixir numeric function for ln
  - F'x/F : is the symbolic derivative of ln, deriv(F,x)/F
  - x*ln(F)-$(x/F),x : is the symbolic integral for ln

  For now the definitions are
  ```
  "ln(F)" => {&:math.log/1, "F'x/F", "x*ln(F)-$(x/F),x"},
  "sin(F)" => {&:math.sin/1, "F'x*cos(F)", nil},
  "cos(F)" => {&:math.cos/1, "-F'x*sin(F)", nil},
  "tan(F)" => {&:math.tan/1, "F'x/cos(F)^2", nil},
  "acos(F)" => {&:math.acos/1, "-F'x/(1-F^2)^0.5", nil},
  "asin(F)" => {&:math.asin/1, "F'x/(1-F^2)^0.5", nil},
  "atan(F)" => {&:math.atan/1, "F'x/(1+F^2)", nil},
  "sinh(F)" => {&:math.sinh/1, "F'x*cosh(F)", nil},
  "cosh(F)" => {&:math.cosh/1, "F'x*sinh(F)", nil},
  "tanh(F)" => {&:math.tanh/1, "F'x/cosh(F)^2", nil},
  "asinh(F)" => {&:math.asinh/1, "F'x/(F^2+1)^0.5", nil},
  "acosh(F)" => {&:math.acosh/1, "F'x/(F^2-1)^0.5", nil},
  "atanh(F)" => {&:math.atanh/1, "F'x/(1-F^2)", nil}
  ```
  """
  # name => Numeric implementation, Derivate, Integrate
  def base,
    do: %{
      "ln(F)" => {&:math.log/1, "F'x/F", "x*ln(F)-$(x/F),x"},
      "exp(F)" => {&:math.exp/1, "F'x*exp(F)",nil},
      "sin(F)" => {&:math.sin/1, "F'x*cos(F)", nil},
      "cos(F)" => {&:math.cos/1, "-F'x*sin(F)", nil},
      "tan(F)" => {&:math.tan/1, "F'x/cos(F)^2", nil},
      "acos(F)" => {&:math.acos/1, "-F'x/(1-F^2)^0.5", nil},
      "asin(F)" => {&:math.asin/1, "F'x/(1-F^2)^0.5", nil},
      "atan(F)" => {&:math.atan/1, "F'x/(1+F^2)", nil},
      "sinh(F)" => {&:math.sinh/1, "F'x*cosh(F)", nil},
      "cosh(F)" => {&:math.cosh/1, "F'x*sinh(F)", nil},
      "tanh(F)" => {&:math.tanh/1, "F'x/cosh(F)^2", nil},
      "asinh(F)" => {&:math.asinh/1, "F'x/(F^2+1)^0.5", nil},
      "acosh(F)" => {&:math.acosh/1, "F'x/(F^2-1)^0.5", nil},
      "atanh(F)" => {&:math.atanh/1, "F'x/(1-F^2)", nil}
    }
  @doc """
  Compounds is a map that holds synthetic definitions. Function
  revert_compounds is able to look in an ast and identify this list,
  substituting the values.
  For now, the definitions are:
  ```
  "sqrt(F)" => "F^0.5",
  "tan(F)" => "sin(F)/cos(F)"
  ```
  """
  def compounds,
    do: %{
      "sqrt(F)" => "F^0.5",
      "tan(F)" => "sin(F)/cos(F)"
    }

  @doc """
  Finds and exec a function call. Search in base and compounds, if cannot find any
  returns a symbolic fcall.
  """
  def fcall(name, args) do
    # IO.inspect({name, args}, label: "fcall")
    aau = allargs_numbers(args)

    cond do
      (bfunc = base()[name <> "(F)"]) != nil ->
        cond do
          aau -> {:numb, elem(bfunc, 0).(args |> List.first() |> elem(1))}
          true -> {:fcall, name, args}
        end

      (cfunc = compounds()[name <> "(F)"]) != nil ->
        {ast, _ctx} = Exun.parse(cfunc)
        replace_args_internal(ast, args, {:vari, "x"})

      true ->
        {:fcall, name, args}
    end
  end
  @doc """
  Replace arguments on ast as specified. Not intended for
  external use, mus be public for now
  """
  def replace_args_internal(ast, args, vari) do
    case ast do
      {:vari, "F"} ->
        args |> List.first()

      {:vari, "x"} ->
        vari

      {:fcall, name, [{:vari, "F"}]} ->
        {:fcall, name, args}

      {:unit, uv, ut} ->
        {:unit, replace_args_internal(uv, args, vari), ut}

      {{:m, op}, lst} ->
        {{:m, op}, Enum.map(lst, &replace_args_internal(&1, args, vari))}

      {:minus, a} ->
        {:minus, replace_args_internal(a, args, vari)}

      {op, l, r} ->
        {op, replace_args_internal(l, args, vari), replace_args_internal(r, args, vari)}

      _ ->
        ast
    end
  end

  defp allargs_numbers(args) do
    Enum.reduce(args, true, fn el, ac ->
      ac and elem(el, 0) == :numb
    end)
  end
  @doc """
  For convenience, creates ast {{:m,:mult},[a,b]}
  """
  def mult(a, b), do: mcompose(:mult, a, b)
  @doc """
  For convenience, creates ast {{:m,:mult},[a,b^-1]}
  """
  def divi(a, b), do: mult(a, Exun.Math.chpow(b))
  @doc """
  For convenience, creates ast {{:m,:suma},[a,b]}
  """
  def suma(a, b), do: mcompose(:suma, a, b)
  @doc """
  For convenience, creates ast {{:m,:mult},[a,-b]}
  """
  def rest(a, b), do: suma(a, Exun.Math.chsign(b))

  @doc """
  For convenience, creates ast {:elev,a,b}
  """
  def elev(a, b), do: {:elev,a,b}

  defp mcompose(op, a, b) do
    case {a, b} do
      {{{:m, ^op}, l1}, {{:m, ^op}, l2}} ->
        Exun.Collect.coll({{:m, op}, l1 ++ l2})

      {{{:m, ^op}, l1}, _} ->
        Exun.Collect.coll({{:m, op}, [b | l1]})

      {_, {{:m, ^op}, l2}} ->
        Exun.Collect.coll({{:m, op}, [a | l2]})

      _ ->
        Exun.Collect.coll({{:m, op}, [a, b]})
    end
  end

  @doc """
  Looks in compounds for a match of ast and return it.
  Thw first wins.
  """
  def revert_compounds(ast) do
    matches =
      compounds()
      |> Enum.map(fn {k, v} ->
        #Compile value
        {vast, _} = Exun.parse(v, %{})
        #Match, get the first match
        res = Exun.Pattern.match_ast(vast, ast, [], false)
        {k, res |> List.first()}
      end)
      |> Enum.reject(fn {_, v} -> v == nil end)

    match = matches |> List.first()

    if match != nil do
      {tk, {:ok, map}} = match
      {atk, _} = Exun.parse(tk, %{})
      replace_args_internal(atk, [Map.fetch!(map, {:vari, "F"})], nil)
    else
      nil
    end
  end

  @doc """
  returns true if ast is a polynomial element on v
  """
  def is_poly(ast,v) do
    case ast do
      {:numb, _} -> true
      {:unit, _,_} -> true
      {:vari, _} -> true
      {:fcall,_,args} -> not Enum.any?(args, &contains(&1,v))
      {:minus, a } -> is_poly(a, v)
      {:deriv, f, _} -> not contains(f,v)
      {:integ, f, _} -> not contains(f,v)
      {{:m,:suma}, list} -> Enum.all?(list, &is_poly(&1,v))
      {{:m,:mult}, list} -> Enum.all?(list, &is_poly(&1,v))
      {:elev, _, b} -> not contains(b,v)
    end
  end
  @doc """
  returns true if ast contains variable vc
  """
  def contains(ast, vc) do
    case ast do
      {:numb, _} -> false
      {:unit, _,_} -> false
      v={:vari, _} -> v==vc
      {:fcall,_,args} -> Enum.any?(args, &contains(&1,vc))
      {:minus, a } -> contains(a, vc)
      {:deriv, f, _} -> contains(f,vc)
      {:integ, f, _} ->  contains(f,vc)
      {{:m,:suma}, list} -> Enum.any?(list,&contains(&1,vc))
      {{:m,:mult}, list} -> Enum.any?(list,&contains(&1,vc))
      {:elev, a, b} -> contains(a,vc) or contains(b,vc)
    end
  end

end
