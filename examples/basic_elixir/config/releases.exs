import Config

config :opentelemetry,
       :processors,
       otel_batch_processor: %{
         # Using `otel` here since we are starting through docker-compose where
         # otel refer to the hostname of the OpenCollector,
         #
         # If you are running it locally, kindly change it to the correct
         # hostname such as `localhost`, `0.0.0.0` and etc.
         exporter: {:opentelemetry_exporter, %{endpoints: [{:http, 'otel', 55681, []}]}}
       }
