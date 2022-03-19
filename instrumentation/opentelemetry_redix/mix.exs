defmodule OpentelemetryRedix.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_redix,
      description: description(),
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_redix"
    ]
  end

  defp description do
    "Trace Redix queries with OpenTelemetry."
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_redix",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  def application do
    [
      mod: {OpentelemetryRedix.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:ex_doc, "~> 0.25.0", only: [:dev], runtime: false},
      {:opentelemetry, "~> 1.0.0", only: [:test]},
      {:opentelemetry_api, "~> 1.0.0"},
      {:opentelemetry_exporter, "~> 1.0.0", only: [:test]},
      {:redix, "~> 1.0", only: [:dev, :test]},
      {:telemetry, "~> 0.4 or ~> 1.0.0"}
    ]
  end
end
