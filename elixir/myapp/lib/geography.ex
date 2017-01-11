
defmodule Country do
  defstruct name: :nil, language: :nil, cities: []
end

defmodule City do
  defstruct name: :nil, pop: :nil, lat: :nil, lon: :nil  
end

defprotocol Valid do
  @doc "returns true if valid"
  def valid?(data)
end

defimpl Valid, for: City do
  def valid?(c) do
    c.pop >= 0 and
    c.lat >= -90.0 and c.lat <= 90.0 and
    c.lon >= -90.0 and c.lon <= 90.0
  end
end

defimpl Inspect, for: Country do
  import Inspect.Algebra
  def inspect(item, _options) do
    pop = concat(to_string(item.pop)," inhabitants")
    msg = concat([item.name, break, pop, " - ", break, to_string(item.lat), break, to_string(item.lon)])
    "wah banana"
  end
end

defmodule Geography do

  # Country - language, [City]
  # City - [name, population, lat, long]

  def make_geo_list(filename) do
    {result, device} = File.open(filename, [:read, :utf8])
    res = []
    process_line(device, res)
  end

  defp process_line(device, res) do
    data = IO.read(device, :line)
    case data do
      :eof  ->  File.close(device)
                res
      _     ->  new_res = parse_line(String.split(String.strip(data), ~r/,/), res)
                process_line(device, new_res)
    end
  end

  defp parse_line([country, language], res) do
    [%Country{name: country, language: language} | res]
  end

  defp parse_line([city, pop, lon, lat], res) do
    [country | t] = res
    c = %City{name: city, pop: String.to_integer(pop), lon: String.to_float(lon), lat: String.to_float(lat)}
    new_country = %{country | cities: [c|country.cities]}
    [new_country | t]
  end

  def total_population([%Country{language: lan, cities: cit} | countries], language) do
    IO.puts("lan #{lan}, language #{language}")
    case language do
      ^lan  -> s(cit, 0)
      _         -> total_population(countries, language)
    end
  end

  defp s([], acc), do: acc

  defp s([city | cities], acc) do
    s(cities, acc+city.pop)
  end

  def tp(), do: total_population(make_geo_list("geography.csv"), "Korean")

end
