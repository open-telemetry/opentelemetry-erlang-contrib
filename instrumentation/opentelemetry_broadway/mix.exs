defmodule OpentelemetryBroadway.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_broadway,
      description: description(),
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_broadway"
    ]
  end

  defp description do
    "OpenTelemetry tracing for Broadway"
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_broadway",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:broadway, "~> 1.0", optional: true},
      {:broadway_kafka, "~> 0.3.0", optional: true},
      {:broadway_rabbitmq, "~> 0.7.0", optional: true},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.28.0", only: [:dev], runtime: false},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_api, "~> 1.0"},
      {:opentelemetry_exporter, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_telemetry, "~> 1.0"},
      {:telemetry, "~> 0.4 or ~> 1.0"}
    ]
  end
end
