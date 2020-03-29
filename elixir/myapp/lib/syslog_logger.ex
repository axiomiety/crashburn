defmodule SyslogLogger do

  @behaviour :gen_event

  def init({__MODULE__, portnumber}) do
    IO.puts("init received #{inspect portnumber}")
    state = %{socket: nil, port: nil}
    {:ok, init(portnumber, state)}
  end

  defp init(portnumber, state) do
    {:ok, socket} = :gen_udp.open(portnumber, [:binary, reuseaddr: true])
    IO.puts("connected to #{portnumber} with socket #{inspect socket}")
    %{state | port: portnumber, socket: socket}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, state) do
    IO.puts("sending #{msg} to syslog")
    ret = :gen_udp.send(state.socket, {127,0,0,1}, state.port, "<14>#{msg}")
    {:ok, state}
  end

  def handle_event(_event, state) do
    IO.puts("handling event #{inspect _event}")
    {:ok, state}
  end

  def handle_info(_info, state) do
    {:ok, state}
  end

  def terminate(_reason, %{socket: socket} = state) do
    IO.puts("closing the socket: #{inspect socket}")
    :gen_udp.close(socket)
  end

end
