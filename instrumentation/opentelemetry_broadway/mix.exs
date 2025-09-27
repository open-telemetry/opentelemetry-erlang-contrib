defmodule OpentelemetryBroadway.MixProject do
  use Mix.Project

  @version "0.3.0"

  def project do
    [
      app: :opentelemetry_broadway,
      version: @version,
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      docs: [
        source_url_pattern:
          "https://github.com/opentelemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_broadway/%{path}#L%{line}",
        main: "OpentelemetryBroadway",
        extras: ["README.md"]
      ],
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: [
        name: "opentelemetry_broadway",
        description: "OpenTelemetry tracing for Broadway",
        maintainers: ["Tom Taylor"],
        licenses: ["Apache-2.0"],
        files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
        source_url:
          "https://github.com/opentelemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_broadway",
        links: %{
          "GitHub" =>
            "https://github.com/opentelemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_broadway"
        }
      ]
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
      {:broadway, "~> 1.0"},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:telemetry, "~> 1.0"},
      {:opentelemetry, "~> 1.5", only: [:test]},
      {:opentelemetry_exporter, "~> 1.8", only: [:test]},
      {:ex_doc, "~> 0.38", only: [:dev], runtime: false}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
