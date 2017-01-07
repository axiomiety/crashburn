defmodule Dates do

  def date_parts(str) do
    Enum.map(String.split(str, "-"), fn x -> String.to_integer(x) end)
  end

end
