defmodule NonFP do

  def generate_pockets(o, prob) do
    IO.puts("what's going on here")
    generate_pockets(o, prob, [])
  end

  def generate_pockets([], _, acc) do
    Enum.reverse(acc)
  end

  def generate_pockets([h|t], prob, acc) when h == ?T do
    generate_pockets(t, prob, [generate_tooth(prob) | acc])
  end

  def generate_pockets([_|t], prob, acc) do
    generate_pockets(t, prob, [[0]|acc])
  end

  def generate_tooth(prob) do
    r = :random.uniform()
    base_depth = cond do
      r < prob  -> 2
      true      -> 3
    end
    generate_tooth(base_depth, 6, [])
  end

  defp generate_tooth(_, 0, acc) do
    Enum.reverse(acc)
  end

  defp generate_tooth(base_depth, num, acc) do
    new_base_depth = base_depth + :random.uniform(3) - 2 # we want a number between -1 and 1
    generate_tooth(base_depth, num-1, [new_base_depth | acc])
  end

end
