import Config

config :logger, level: :debug

config :opentelemetry,
  processors: [{:otel_simple_processor, %{}}]

config :ex_aws,
  json_codec: Test.JSONCodec,
  access_key_id: "testkeyid",
  secret_access_key: "secretaccesskey"

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

config :ex_aws, :s3,
  scheme: "http://",
  host: "localhost",
  port: 9000,
  region: "us-east-1"
