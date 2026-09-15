defmodule OpentelemetryPhoenix.TestSupport.LiveViewMeta do
  @moduledoc false

  alias OpentelemetryPhoenix.TestSupport.MyTestLive
  alias OpentelemetryPhoenix.TestSupport.Router

  def socket do
    %Phoenix.LiveView.Socket{
      id: "phx-F5LbkYMazc6nbROF",
      endpoint: OpentelemetryPhoenix.TestSupport.Endpoint,
      router: Router,
      view: MyTestLive,
      assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
      private: %{live_temp: %{}, root_view: MyTestLive},
      host_uri: URI.parse("http://localhost:4000")
    }
  end

  def mount_start do
    %{
      socket: socket(),
      params: %{"foo" => "bar"},
      session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"},
      uri: "http://localhost:4000/live?foo=bar"
    }
  end

  def put_uri(meta, uri), do: %{meta | uri: uri}

  def put_router(meta, router), do: %{meta | socket: %{meta.socket | router: router}}
end
