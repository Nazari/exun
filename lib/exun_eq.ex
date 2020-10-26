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
    :number => 0,
    :vari => 1,
    :unit => 2,
    :integ => 3,
    :deriv => 4,
    {:m, :sum} => 5,
    {:m, :mult} => 6,
    :minus => 7,
    :elev => 8
  }

  def smm(l, r) do
    lt = @stype[elem(l, 0)]
    rt = @stype[elem(r, 0)]

    cond do
      lt == rt -> l < r
      true -> lt < rt
    end
  end
end
