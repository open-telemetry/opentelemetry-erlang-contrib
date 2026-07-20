defmodule OpentelemetryCommanded.DummyApp.App do
  alias Commanded.EventStore.Adapters.InMemory
  alias Commanded.Serialization.JsonSerializer
  alias OpentelemetryCommanded.DummyApp.Router

  use Commanded.Application,
    otp_app: :commanded,
    event_store: [
      adapter: InMemory,
      serializer: JsonSerializer
    ],
    pubsub: :local,
    registry: :local

  router(Router)
end
