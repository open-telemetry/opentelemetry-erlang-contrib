defmodule OpentelemetryDataloader.MixProject do
  use Mix.Project

  @version "1.0.0"

  def project do
    [
      app: :opentelemetry_dataloader,
      description: "Trace Dataloader with OpenTelemetry.",
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_dataloader",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_dataloader/%{path}#L%{line}",
        main: "OpentelemetryDataloader",
        extras: ["README.md"]
      ]
    ]
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_dataloader",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
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
    [test: ["ecto.drop -q", "ecto.create -q", "ecto.migrate --quiet", "test"]]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:telemetry, "~> 1.0"},
      {:opentelemetry_api, "~> 1.2"},
      {:opentelemetry_telemetry, "~> 1.0"},
      {:dataloader, "~> 2.0", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:ex_doc, "~> 0.31", only: [:dev], runtime: false},
      {:ecto_sql, ">= 3.0.0", only: [:dev, :test]},
      {:postgrex, ">= 0.15.0", only: [:dev, :test]},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false},
      {:opentelemetry_process_propagator, "~> 0.2.1"}
    ]
  end
end
