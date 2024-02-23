defmodule ServerHelper do
  defmacro __using__(_) do
    quote location: :keep do
      import Plug.Conn

      def init(opts) do
        opts
      end

      def call(conn, []) do
        function = String.to_atom(List.first(conn.path_info))
        apply(__MODULE__, function, [conn])
      end

      defoverridable init: 1, call: 2
    end
  end
end
