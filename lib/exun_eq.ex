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
    :numb => 1,
    :vari => 2,
    :fcall => 3,
    :unit => 4,
    :integ => 5,
    :deriv => 6,
    {:m, :sum} => 7,
    {:m, :mult} => 8,
    :elev => 9,
    :minus => 10,
  }
  @doc """
  Sort for m multiple list
  Sorts components of lis {{:m,op},lst} in a convenient way for eq
  and normalizing
  """
  def smm(left, right) do
    type_l = get_first_of_tuple(left)
    type_r = get_first_of_tuple(right)

    left_index = @stype[type_l]
    right_index = @stype[type_r]

    cond do
      left_index == right_index -> left < right
      true -> left_index < right_index
    end
  end

  defp get_first_of_tuple(tup) do
    elem(tup, 0)
  end
end
