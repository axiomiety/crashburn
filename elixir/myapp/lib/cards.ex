defmodule Cards do

  def make_deck do
    for rank <- ["C","D","H","S"], suit <- ["A",2,3,4,5,6,7,8,9,"J","Q","K"], do: {suit, rank}
  end

end
