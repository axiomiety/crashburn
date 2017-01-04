defmodule Disjkstra do

  def gcd(m,m) do
    m
  end

  def gcd(m,n) when m > n do
    gcd(m-n, n)
  end

  def gcd(m,n) do
    gcd(m, n-m)
  end

end
