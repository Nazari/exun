defmodule Exun.Eq do
  @moduledoc """
  Compares two expressions
  """
  @doc """
  Tree equality, normalize compounds '*' and '+' because
  {*,{*,1,2},{*,3,4}} == {*,{*,1,3},{*,2,4}}
  so transform both trees to {{:m,*}[1,2,3,4]} before compare
  """
  def eq(t1, t2) do
    t1 == t2
  end

  @stype %{
    :numb => 0,
    :vari => 1,
    :fcall => 2,
    :unit => 3,
    :integ => 4,
    :deriv => 5,
    {:m, :sum} => 6,
    {:m, :mult} => 7,
    :minus => 8,
    :elev => 9
  }
  @doc """
  Sort for m multiple list
  Sorts components of lis {{:m,op},lst} in a convenient way for eq
  and operating
  """
  def smm(l, r) do
    tl = grt(l)
    tr = grt(r)

    lt = @stype[tl]
    rt = @stype[tr]

    cond do
      lt == rt -> l < r
      true -> lt < rt
    end
  end

  defp grt(tup) do
    t = elem(tup, 0)

    if t == :minus do
      #IO.inspect(tup,label: "tupla")
      elem(elem(tup, 1), 0)
    else
      t
    end
  end
end
