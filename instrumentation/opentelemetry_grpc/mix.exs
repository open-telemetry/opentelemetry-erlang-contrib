defmodule OpentelemetryGrpc.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :opentelemetry_grpc,
      description: description(),
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      dialyzer: [
        plt_add_apps: [:ex_unit, :mix],
        plt_core_path: "priv/plts",
        plt_local_path: "priv/plts"
      ],
      deps: deps(),
      name: "Opentelemetry gRPC",
      docs: [
        main: "OpentelemetryGrpc",
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_grpc/%{path}#L%{line}",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_grpc",
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp description do
    "Trace gRPC requests and responses with OpenTelemetry."
  end

  defp package do
    [
      description: "OpenTelemetry tracing for gRPC",
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_grpc",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support", "test/support/proto"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:nimble_options, "~> 1.1"},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:telemetry, "~> 1.0"},
      {:grpc, "~> 0.8"},
      {:protobuf, "~> 0.15"},
      {:opentelemetry_exporter, "~> 1.8", only: [:dev, :test]},
      {:opentelemetry, "~> 1.5", only: [:dev, :test]},
      {:ex_doc, "~> 0.38", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false}
    ]
  end

  defp aliases do
    [
      "protoc.compile": [
        "cmd protoc --elixir_out=plugins=grpc:test/support/proto --proto_path=proto proto/test_service.proto"
      ],
      test: ["test --warnings-as-errors --trace"]
    ]
  end
end
