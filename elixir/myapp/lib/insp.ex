
defmodule Foo do
  defstruct name: :nil  
end


defimpl Valid, for: Foo do
  def valid?(c) do
    c.name != :nil
  end
end

defimpl String.Chars, for: Foo do
  def to_string(f) do
    "This is foo, as #{f.name}"
  end
end

defimpl Inspect, for: Foo do
  import Inspect.Algebra
  def inspect(item, _options) do
    "wah banana"
  end
end
