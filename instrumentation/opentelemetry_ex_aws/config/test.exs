import Config

config :logger, level: :debug

config :opentelemetry,
  processors: [{:otel_simple_processor, %{}}]

config :ex_aws,
  json_codec: Test.JSONCodec,
  access_key_id: ["test_key_id"],
  secret_access_key: ["secret_access_key"]

config :ex_aws, :dynamodb,
  scheme: "http://",
  host: "localhost",
  port: 8000,
  region: "us-east-1"

config :ex_aws, :dynamodb_streams,
  scheme: "http://",
  host: "localhost",
  port: 8000,
  region: "us-east-1"
