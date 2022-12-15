import Config

# config/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.

# Start the phoenix server if environment is set and running in a release
if System.get_env("PHX_SERVER") && System.get_env("RELEASE_NAME") do
  config :demo, DemoWeb.Endpoint, server: true
end

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6"), do: [:inet6], else: []

  config :demo, Demo.Repo,
    # ssl: true,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    socket_options: maybe_ipv6

  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "example.com"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :demo, DemoWeb.Endpoint,
    url: [host: host, port: 443],
    http: [
      # Enable IPv6 and bind on all interfaces.
      # Set it to  {0, 0, 0, 0, 0, 0, 0, 1} for local network only access.
      # See the documentation on https://hexdocs.pm/plug_cowboy/Plug.Cowboy.html
      # for details about using IPv6 vs IPv4 and loopback vs public addresses.
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  config :opentelemetry, :processors,
    otel_batch_processor: %{
      # Using `otel` here since we are starting through docker-compose where
      # otel refer to the hostname of the OpenCollector,
      #
      # If you are running it locally, kindly change it to the correct
      # hostname such as `localhost`, `0.0.0.0` and etc.
      exporter: {:opentelemetry_exporter, %{endpoints: ["http://otel:4318"]}}

      # Example configuration to directly export to Honeycomb.io
      # exporter: {
      #   :opentelemetry_exporter, %{
      #     endpoints: ["https://api.honeycomb.io:443"],
      #     headers: [
      #       {"x-honeycomb-team", System.get_env("HONEYCOMB_TEAM")},
      #       {"x-honeycomb-dataset", System.get_env("HONEYCOMB_DATASET") || "demo"}
      #     ]
      #   }
      # }

      # Example configuration to directly export to Lightstep.com
      #
      # Since Lightstep has a different OTLP trace endpoint, we are going need
      # to configure it in a separate configuration.
      # exporter: {
      #   :opentelemetry_exporter, %{}
      # }
    }

  # Example configuration for Lightstep.com, for more refers to:
  # https://github.com/open-telemetry/opentelemetry-erlang/tree/main/apps/opentelemetry_exporter#application-environment
  # config :opentelemetry_exporter,
  #   # You can also configure the compression type for exporting traces.
  #   oltp_traces_compression: :gzip,
  #   otlp_traces_endpoint: "https://ingest.lightstep.com:443/traces/otlp/v0.9",
  #   otlp_headers: [
  #     {"lightstep-access-token", System.get_env("LIGHTSTEP_ACCESS_TOKEN")}
  #   ]

  # ## Using releases
  #
  # If you are doing OTP releases, you need to instruct Phoenix
  # to start each relevant endpoint:
  #
  #     config :demo, DemoWeb.Endpoint, server: true
  #
  # Then you can assemble a release by calling `mix release`.
  # See `mix help release` for more information.

  # ## Configuring the mailer
  #
  # In production you need to configure the mailer to use a different adapter.
  # Also, you may need to configure the Swoosh API client of your choice if you
  # are not using SMTP. Here is an example of the configuration:
  #
  #     config :demo, Demo.Mailer,
  #       adapter: Swoosh.Adapters.Mailgun,
  #       api_key: System.get_env("MAILGUN_API_KEY"),
  #       domain: System.get_env("MAILGUN_DOMAIN")
  #
  # For this example you need include a HTTP client required by Swoosh API client.
  # Swoosh supports Hackney and Finch out of the box:
  #
  #     config :swoosh, :api_client, Swoosh.ApiClient.Hackney
  #
  # See https://hexdocs.pm/swoosh/Swoosh.html#module-installation for details.
end
