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
  def smm(left, right) do
    type_l = grt(left)
    type_r = grt(right)

    left_index = @stype[type_l]
    right_index = @stype[type_r]

    cond do
      left_index == right_index -> left < right
      true -> left_index < right_index
    end
  end

  defp grt(tup) do
    elem(tup, 0)
  end
end
