defmodule OpentelemetryOban.MixProject do
  use Mix.Project

  @version "1.2.0"

  def project do
    [
      app: :opentelemetry_oban,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      elixirc_paths: elixirc_paths(Mix.env()),
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_oban/%{path}#L%{line}",
        main: "OpentelemetryOban",
        extras: ["README.md"]
      ],
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
      ],
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_oban"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp aliases() do
    [test: ["ecto.drop -q", "ecto.create -q", "ecto.migrate --quiet", "test"]]
  end

  defp deps do
    [
      {:oban, "~> 2.0"},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:opentelemetry, "== 1.7.0", only: [:test]},
      {:opentelemetry_exporter, "== 1.10.0", only: [:test]},
      {:nimble_options, "~> 1.1"},
      {:telemetry, "~> 1.0"},
      {:jason, "== 1.4.5", only: [:dev, :test]},
      {:ex_doc, "== 0.40.3", only: [:dev], runtime: false},
      {:postgrex, "== 0.22.3", only: [:dev, :test]}
    ]
  end
end
