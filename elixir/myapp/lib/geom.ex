defmodule Geom do
  @moduledoc """
  This is a module to help calculate basic geometric properties
  """

  @doc """
  This function calculates the area of a rectangle.
  """

  # @spec area(number, number) :: number
  # def area(h \\ 1, w \\ 1) when h >=0 and w >= 0 do
  #  h*w
  # end
  def area({shape, a, b}) do
    area(shape, a, b)
  end

  def area(s, a, b) when a >= 0 and b >= 0 do
    case s do
      :triangle -> a*b/2.0
      :rectangle -> a*b
      :ellipse -> :math.pi()*a*b
    end
  end


  def weird_sum(a\\3, b, c\\7) do
    a+b+c
  end

end
