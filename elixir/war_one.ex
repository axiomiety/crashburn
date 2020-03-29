
defmodule Player do

  def loop(name, cards) do
    IO.puts("#{name} has #{Enum.count(cards)} cards")
    receive do
      {:receive, new_cards} -> 
        IO.puts("#{name} received #{Enum.count(new_cards)} from dealer")
        loop(name, cards ++ new_cards)
      {:give, from, num_cards} ->
        IO.puts("#{name} received :take #{num_cards} from #{inspect from}")
        {cards_to_give, remaining_cards} = Enum.split(cards, num_cards)
        send(from, {name, cards_to_give})
        loop(name, remaining_cards)
      {:show} ->
        #TODO this should probably send something back
        IO.puts(Enum.join(["#{name}'s deck:" | Enum.map(cards, &("#{elem(&1,1)}#{elem(&1,0)}"))], " "))
        loop(name, cards)
    end
  end

end

defmodule Dealer do

  def init() do
    deck = Cards.shuffle(Cards.make_deck())
    {dp1, dp2} = Enum.split(deck, 26)
    player1 = spawn(Player, :loop, ["Bob", dp1])
    Process.register(player1, :player1)
    player2 = spawn(Player, :loop, ["Alice", dp2])
    Process.register(player2, :player2)
    loop([], [], 0)
  end

  def loop([], [], 0) do
    send(:player1, {:take, self(), 1})
    send(:player2, {:take, self(), 1})
    receive do
      {"Player 1", cards} ->
        loop(cards, [], 1)
      {"Player 2", cards} ->
        loop([], cards, 1)
    end
  end

  def loop(p1, p2, count) when count < 2 do
    IO.puts("p1: #{inspect p1}, p2: #{inspect p2}")
    receive do
      {"Player 1", cards} ->
        loop(cards, p2, count + 1)
      {"Player 2", cards} ->
        loop(p1, cards, count + 1)
    end
  end

  def loop([], p2, count) when count == 2 do
    IO.puts("p1: [], p2: #{inspect p2} - P2 wins!")
  end

  def loop(p1, [], count) when count == 2 do
    IO.puts("p1: #{inspect p1}, p2: [] - P1 wins!")
  end
  
  def loop([h1|t1], [h2|t2], count) when count == 2 do
    IO.puts("p1: #{inspect h1}, p2: #{inspect h2}")
    
    {_, v1} = h1
    {_, v2} = h2
    cond do
      (v1 > v2) ->
        IO.puts("p1 wins this round as #{v1} > #{v2}")
        send(:player1, {:add, [h1|t1] ++ [h2|t2]})
        loop([], [], 0)
      (v2 > v1) ->
        IO.puts("p2 wins this round as #{v2} > #{v1}")
        send(:player1, {:add, [h2|t2] ++ [h1|t1]})
        loop([], [], 0)
      (v1 == v2) -> 
        IO.puts("tie! as #{v1} == #{v2}")
        send(:player1, {:take, self(), 3})
        send(:player2, {:take, self(), 3})
        loop([h1|t1], [h2|t2], 0)
    end
  end

end

defmodule Cards do
  @moduledoc "Represents a deck of 52 cards"
  
  def shuffle(cards), do: cards

  @doc "A, K, Q, J are 13, 12, 11, 10 respectively"
  def make_deck do
    for suit <- ["C","D","H","S"], rank <- 2..13, do: {suit, rank}
  end
end
