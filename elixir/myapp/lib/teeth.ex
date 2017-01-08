defmodule Teeth do

  @moduledoc "foo"

  import Stats

  def alert(teeth) do
    alert(teeth, [], 0)
  end

  defp alert([], acc, _) do
    Enum.reverse(acc)
  end

  defp alert([[0]|t], acc, idx) do
    alert(t, acc, idx+1)
  end

  defp alert([h|t], acc, idx) do
    cond do
      Stats.maximum(h) > 3 -> alert(t, [idx | acc], idx+1)
      true -> alert(t, acc, idx+1)
    end
  end

end
