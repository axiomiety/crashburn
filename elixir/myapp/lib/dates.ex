defmodule Dates do

  @moduledoc "foo"

  def date_parts(str) do
    Enum.map(String.split(str, "-"), fn x -> String.to_integer(x) end)
  end

  @doc "calc stuff"
  def julian(str) do
    [y,m,d] = date_parts(str)
    leap_offset = cond do
      is_leap_year(y) and m >= 2  -> 1
      true                        -> 0
    end
    d + month_total(m, [31,28,31,30,31,30,31,31,30,31,30,31], 0) + leap_offset
  end

  def month_total(1, _, acc), do: acc

  def month_total(m, [h|t], acc), do: month_total(m-1, t, acc+h)

  def is_leap_year(year) do
    (rem(year,4) == 0 and rem(year, 100) != 0)
    or (rem(year, 400) == 0)
  end
end
