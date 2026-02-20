defmodule OpentelemetryEcto.MixProject do
  use Mix.Project

  @version "2.0.0-beta.1"

  def project do
    [
      app: :opentelemetry_ecto,
      description: description(),
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      dialyzer: [
        plt_add_apps: [:ex_unit, :mix],
        plt_core_path: "plts",
        plt_local_path: "plts"
      ],
      deps: deps(),
      aliases: aliases(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_ecto",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_ecto/%{path}#L%{line}",
        main: "OpentelemetryEcto",
        extras: ["README.md"]
      ]
    ]
  end

  defp description do
    "Trace Ecto queries with OpenTelemetry."
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_ecto",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" => "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  def application do
    []
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp aliases() do
    [
      test: [
        "ecto.drop -q",
        "ecto.create -q",
        "test"
      ]
    ]
  end

  defp deps do
    [
      {:nimble_options, "~> 1.0"},
      {:telemetry, "~> 1.0"},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_process_propagator, "~> 0.3"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:opentelemetry, "~> 1.5", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.8", only: [:dev, :test]},
      {:ex_doc, "~> 0.38", only: [:dev], runtime: false},
      {:ecto_sqlite3, "~> 0.21", only: [:dev, :test]},
      {:ecto_sql, "~> 3.12", only: [:dev, :test]},
      {:postgrex, "~> 0.20", only: [:dev, :test]},
      {:myxql, "~> 0.7", only: [:dev, :test]},
      {:tds, "~> 2.3", only: [:dev, :test]},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
    ]
  end
end
