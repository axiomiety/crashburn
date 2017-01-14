defmodule Calculus do
  
  def derivative(f, pt) do
    delta = 1.0e-10
    (f.(pt + delta) - f.(pt))/delta
  end

end
