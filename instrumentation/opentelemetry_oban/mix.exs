defmodule OpentelemetryOban.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_oban,
      version: "1.0.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
        main: "OpentelemetryOban",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: [
        name: "opentelemetry_oban",
        description: "OpenTelemetry tracing for Oban",
        maintainers: ["Glia TechMovers"],
        licenses: ["Apache-2.0"],
        links: %{
          "GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
          "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
          "OpenTelemetry.io" => "https://opentelemetry.io"
        },
        files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*)
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:oban, "~> 2.0"},
      {:opentelemetry_api, "~> 1.0"},
      {:opentelemetry_telemetry, "~> 1.0.0"},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:opentelemetry, "~> 1.0", only: [:test]},
      {:opentelemetry_exporter, "~> 1.0", only: [:test]},
      {:telemetry, "~> 0.4 or ~> 1.0"},
      {:ex_doc, "~> 0.28", only: [:dev], runtime: false}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
