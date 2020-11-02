defmodule Exun.Matrix do
  import Exun.Fun
  import Exun.Collect

  @moduledoc """
  Manage symbolic matrices, simple operations as add, rest, mult and divi; and
  calculate eigenvalues; could be interesting for solve n-polynomies.
  Matrix is a List of lists; each sublist is a row because is best reflected when
  we inspect a list of list in its simples form. But for better process and memory
  usage whe will use a tuple {{:mtype,row,cols},[[],[],[]]} so the first tuple in tuple describes
  the nature of the matrix:
  {{:unity,4,4},nil} is a 4x4 matrix all zeros except de main diagonal that holds 1's.
  {{:polynom,5,5},[c4,c3,c2,c1,c0]} is a matrix 5x5 that reflects polinomial coefficients
  {{:raw,m,n},[[],[],[]]} Normal matrix, all elements in sublists.

  Of course, we will support symbolic matrices, each element of the matrix is an ast
  """
  @uno {:numb, 1}
  @zero {:numb, 0}

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
      |> Enum.map(&divi(&1, h))
      |> Enum.reverse()

    l = length(first_row)
    {{:polynom, l, l}, first_row}
  end

  def get_elem({{type, rows, cols}, values}, row, col) when is_number(row) and is_number(col) do
    if row < 0 or row >= rows or col < 0 or col >= cols or floor(row) != row or floor(col) != col do
      throw("Invalid row or col for matrix get_elem")
    end

    case type do
      :unity ->
        if row == col, do: @uno, else: @zero

      :polynom ->
        case row do
          0 ->
            Enum.at(values, col)

          ^col ->
            if row == rows - 1, do: @zero, else: @uno

          _ ->
            @zero
        end

      :raw ->
        Enum.at(values, row)
        |> elem(1)
        |> Enum.at(col)
    end
  end

  @doc """
  Return row number row from matrix
  """
  def get_row(a = {{type, rows, cols}, val}, row) do
    case type do
      :unity ->
        List.duplicate({:numb, 0}, rows) |> List.replace_at(row, {:numb, 1})

      :polynom ->
        case row do
          0 ->
            val

          _ ->
            for c <- 0..(cols - 1), do: get_elem(a, row, c)
        end

      :raw ->
        for c <- 0..(cols - 1), do: get_elem(a, row, c)
    end
  end

  def get_col(a = {{_, rows, _}, _}, col) do
    for i <- 0..(rows - 1) do
      get_elem(a, i, col)
    end
  end

  def mult_matrix(a = {{_, ra, ca}, _}, b = {{_, rb, cb}, _}) do
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

    {{:raw, ra, cb}, content}
  end

  defp mult_list(rlist, clist) when is_list(rlist) and is_list(clist) do
    coll(
      {{:m, :suma},
       List.zip([clist, rlist])
       |> Enum.map(fn {r, c} -> mult(r, c) end)}
    )
  end
end
