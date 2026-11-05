defmodule LivePeerTCP do
  @moduledoc """
  A TCP transport that looks up the peer on the live socket when Bandit asks for
  peer data, the way Thousand Island did before it cached the peername at accept time.
  """

  @behaviour ThousandIsland.Transport

  alias ThousandIsland.Transports.TCP

  @impl true
  def peercert(socket) do
    case :inet.peername(socket) do
      {:ok, _peername} -> TCP.peercert(socket)
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  defdelegate listen(port, user_options), to: TCP
  @impl true
  defdelegate accept(listener_socket), to: TCP
  @impl true
  defdelegate handshake(socket), to: TCP
  @impl true
  defdelegate handshake(socket, timeout), to: TCP
  @impl true
  defdelegate upgrade(socket, opts), to: TCP
  @impl true
  defdelegate controlling_process(socket, pid), to: TCP
  @impl true
  defdelegate recv(socket, length, timeout), to: TCP
  @impl true
  defdelegate send(socket, data), to: TCP
  @impl true
  defdelegate sendfile(socket, filename, offset, length), to: TCP
  @impl true
  defdelegate getopts(socket, options), to: TCP
  @impl true
  defdelegate setopts(socket, options), to: TCP
  @impl true
  defdelegate shutdown(socket, way), to: TCP
  @impl true
  defdelegate close(socket), to: TCP
  @impl true
  defdelegate sockname(socket), to: TCP
  @impl true
  defdelegate peername(socket), to: TCP
  @impl true
  defdelegate secure?(), to: TCP
  @impl true
  defdelegate getstat(socket), to: TCP
  @impl true
  defdelegate negotiated_protocol(socket), to: TCP
  @impl true
  defdelegate connection_information(socket), to: TCP
end
