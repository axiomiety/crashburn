defmodule AskArea do

  import Geom

  def area() do
    shape = IO.gets("R)ectangle, T)riangle, or E)llipse: ")
    |> String.first()
    |> String.upcase()
    |> char_to_shape()
    calculate(shape, get_dimensions(shape))
  end

  def get_number(prompt) do
    o = IO.gets("Enter #{prompt}> ")
        |> String.strip()

    cond do
      Regex.match?(~r/^[0-9]*$/, o) -> String.to_integer(o)
      true -> :error
    end
  end

  defp get_dimensions(:rectangle), do: get_dimensions("width", "height")

  defp get_dimensions(d1, d2) do
    {get_number(d1), get_number(d2)}
  end

  defp char_to_shape("R"), do: :rectangle 
  defp char_to_shape("T"), do: :triangle 
  defp char_to_shape(s), do: IO.puts("Unknown shape #{s}")

  defp calculate(shape, {d1, d2}) do
    Geom.area(shape, d1, d2)
  end

end

