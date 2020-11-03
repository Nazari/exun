defmodule Exun.Matrix do
  alias Exun.Collect
  alias Exun.Math
  require Integer

  @moduledoc """
  Manage symbolic matrices, simple operations as add, rest, mult and divi; and
  calculate eigenvalues; could be interesting for solve n-polynomies.
  Matrix is a List of lists; each sublist is a row because is best reflected when
  we inspect a list of list in its simples form. But for better process and memory
  usage whe will use a tuple {{:mtype,row,cols},[[],[],[]]} so the first tuple in tuple describes
  the nature of the matrix:
  {{:unity,4,4},{maskedrows,maskedcols},nil} is a 4x4 matrix all zeros except de main diagonal that holds 1's.
  {{:polynom,5,5},{maskedrows,maskedcols},[c4,c3,c2,c1,c0]} is a matrix 5x5 that reflects polinomial coefficients
  {{:raw,m,n},{maskedrows,maskedcols},[[],[],[]]} Normal matrix, all elements in sublists.

  Of course, we will support symbolic matrices, each element of the matrix is an ast
  """
  @uno {:numb, 1,1}
  @zero {:numb, 0,1}

  @doc """
  Return quadratic (nxn) identity matrix
  """
  def uni_m(n) when is_number(n) do
    {{:unity, n, n}, nil}
  end

  def pol_m(coefs) when is_list(coefs) do
    [h | tail] = coefs |> Enum.reverse()

    first_row =
      tail
      |> Enum.map(&Math.divi(&1, h))
      |> Enum.reverse()

    l = length(first_row)
    {{:polynom, l, l}, first_row}
  end

  def get_elem({{type, rows, cols}, values, mr, mc}, row, col)
      when is_number(row) and is_number(col) and is_list(mr) and is_list(mc) do
    if row < 0 or row >= rows or col < 0 or col >= cols or
         floor(row) != row or floor(col) != col do
      throw("Invalid row or col for matrix get_elem")
    end

    rrow = rel_coord(row, mr)
    rcol = rel_coord(col, mc)

    case type do
      :unity ->
        if rrow == rcol, do: @uno, else: @zero

      :polynom ->
        case rrow do
          0 ->
            Enum.at(values, rcol)

          ^rcol ->
            if rrow == rows - 1, do: @zero, else: @uno

          _ ->
            @zero
        end

      :raw ->
        Enum.at(values, rrow)
        |> elem(1)
        |> Enum.at(rcol)
    end
  end

  defp rel_coord(orig, masked) do
    Enum.reduce(masked, orig, fn mask, realcoord ->
      if mask >= realcoord, do: realcoord + 1, else: realcoord
    end)
  end

  @doc """
  Return row number row from matrix
  """
  def get_row(a = {{type, rows, cols}, val, _, _}, row) do
    case type do
      :unity ->
        List.duplicate(@zero, rows) |> List.replace_at(row, @uno)

      :polynom ->
        case row do
          0 ->
            val

          _ ->
            for c <- 0..(cols - 1), do: get_elem(a, row, c)
        end

      :raw ->
        Enum.at(val,row)
        |> elem(1)
    end
  end

  def get_col(a = {{_, rows, _}, _, _, _}, col) do
    for i <- 0..(rows - 1) do
      get_elem(a, i, col)
    end
  end

  def mult_matrix(a = {{_, ra, ca}, _, _, _}, b = {{_, rb, cb}, _, _, _}) do
    if ca != rb, do: throw("Cannot multiply matrices #{ra}x#{ca} and #{rb}x#{cb}")

    content =
      for rown_a <- 0..(ra - 1) do
        row_a = get_row(a, rown_a)

        {{:vector, ra},
         for coln_b <- 0..(cb - 1) do
           col_b = get_col(b, coln_b)
           mult_list(row_a, col_b)
         end}
      end

    {{:raw, ra, cb}, content, [], []}
  end

  defp mult_list(rlist, clist) when is_list(rlist) and is_list(clist) do
    Collect.coll(
      {{:m, :suma},
       List.zip([clist, rlist])
       |> Enum.map(fn {r, c} -> Math.mult(r, c) end)}
    )
  end

  def det(a = {{_, 2, 2}, _, _, _}) do
    a00 = get_elem(a, 0, 0)
    a11 = get_elem(a, 1, 1)
    a01 = get_elem(a, 0, 1)
    a10 = get_elem(a, 1, 0)

    Collect.coll(
      Math.rest(
        Math.mult(a00, a11),
        Math.mult(a01, a10)
      )
    )
  end

  def det(a = {{_, n, n}, _, _, _}) do
    Collect.coll(
      {{:m, :suma},
       for i <- 0..(n - 1) do
         pivot = get_elem(a, 0, i)

         if Integer.is_odd(n) do
           Math.minus(Math.mult(pivot, det(mask(a, 0, i))))
         else
           Math.mult(pivot, det(mask(a, 0, i)))
         end
       end}
    )
  end

  def mask({{t, r, c}, v, mr, mc}, row, col) do
    maskedrows = [row | mr]
    maskedcols = [col | mc]

    {{t, r - length(maskedrows) + length(mr), c - length(maskedcols) + length(mc)}, v, maskedrows,
     maskedcols}
  end

  def unmask({{t, r, c}, v, mr, mc}, row, col) do
    maskedrows = List.delete(mr, row)
    maskedcols = List.delete(mc, col)

    {{t, r - length(maskedrows) + length(mr), c - length(maskedrows) + length(mc)}, v, maskedrows,
     maskedcols}
  end
end
