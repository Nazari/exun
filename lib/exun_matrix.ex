defmodule Exun.Matrix do
  import Exun.Fun

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
end
