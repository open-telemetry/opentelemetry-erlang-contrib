Application.ensure_all_started(:opentelemetry)
Application.ensure_all_started(:opentelemetry_api)

defmodule TestServer do
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  match "/headers" do
    conn
    |> Plug.Conn.put_resp_header("resp-header", "1")
    |> send_resp(200, "ok")
  end

  match "/status/:status_code" do
    send_resp(conn, String.to_integer(status_code), "Here's that status code you ordered!")
  end

  match _ do
    send_resp(conn, 200, "It's polite to reply!")
  end
end

child_spec = [{Plug.Cowboy, scheme: :http, plug: TestServer, options: [port: 8000]}]
{:ok, _pid} = Supervisor.start_link(child_spec, strategy: :one_for_one)

ExUnit.start()
