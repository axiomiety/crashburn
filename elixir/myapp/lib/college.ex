defmodule College do

  def read_file(filename) do
    {result, device} = File.open(filename, [:read, :utf8])
    ds = HashDict.new()
    process(device, ds)
  end

  def process(device, ds) do
    data = IO.read(device, :line)
    case data do
      :eof  ->  File.close(device)
                ds
      _     ->  new_ds = process_room(data, ds)
                process(device, new_ds)
    end
  end

  def process_room(data, ds) do
    IO.puts("line: #{inspect data}")
    [_course_id, course_name, room] = String.split(String.strip(data), ~r/,/)
    courses_by_room = HashDict.get(ds, room)
    case courses_by_room do
      nil -> updated_ds = HashDict.put_new(ds, room, [course_name])
      _   -> updated_ds = HashDict.put(ds, room, [course_name | courses_by_room])
    end
    updated_ds
  end

end
