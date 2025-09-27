defmodule OpentelemetryNebulex.MixProject do
  use Mix.Project

  @version "0.2.0"

  def project do
    [
      app: :opentelemetry_nebulex,
      description: description(),
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_nebulex",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_nebulex/%{path}#L%{line}",
        main: "OpentelemetryNebulex",
        extras: ["README.md"]
      ]
    ]
  end

  defp description do
    "OpenTelemetry tracing for Nebulex"
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_nebulex",
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
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.38", only: [:dev], runtime: false},
      {:nebulex, "~> 2.1", only: [:dev, :test]},
      {:opentelemetry, "~> 1.5", only: [:dev, :test]},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_exporter, "~> 1.8", only: [:dev, :test]},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:telemetry, "~> 1.0"}
    ]
  end
end
