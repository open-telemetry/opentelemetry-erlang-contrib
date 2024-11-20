defmodule OtelTelemetryMetrics.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_telemetry_metrics,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url: "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
      homepage_url: "http://github.com/open-telemetry/opentelemetry-erlang-contrib",
      docs: [main: "OtelTelemetryMetrics",
             extras: ["README.md"]]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:telemetry, "~> 1.3"},
      {:telemetry_metrics, "~> 1.0"},
      {:opentelemetry_api, "~> 1.3"},
      {:opentelemetry_api_experimental, "~> 0.5"},
      {:opentelemetry, "~> 1.4", only: [:dev, :test]},
      {:opentelemetry_experimental, "~> 0.5", only: [:dev, :test]},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end
end
