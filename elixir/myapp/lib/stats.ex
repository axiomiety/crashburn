defmodule Stats do

  def minimum([h|t]), do: minimum(t,h)

  def minimum([], m), do: m

  def minimum([h|t], m) do
    cond do
      h < m -> minimum(t, h)
      true  -> minimum(t, m)
    end
  end

  def maximum([h|t]), do: maximum(t,h)

  def maximum([], m), do: m

  def maximum([h|t], m) when h < m, do: maximum(t,m)
  def maximum([h|t], _m), do: maximum(t,h)

  @doc "Returns the minimum and maximum of a list"
  @spec range([number]) :: [number]
  def range(data), do: [minimum(data), maximum(data)]

end
