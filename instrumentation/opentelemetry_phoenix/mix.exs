defmodule OpentelemetryPhoenix.MixProject do
  use Mix.Project

  @version "2.0.0"

  def project do
    [
      app: :opentelemetry_phoenix,
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
      name: "Opentelemetry Phoenix",
      docs: [
        main: "OpentelemetryPhoenix",
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_phoenix/%{path}#L%{line}",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_phoenix"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  defp description do
    "Trace Phoenix requests with OpenTelemetry."
  end

  defp package do
    [
      description: "OpenTelemetry tracing for the Phoenix Framework",
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_phoenix",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" => "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nimble_options, "~> 1.0"},
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_process_propagator, "~> 0.3"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:otel_http, "~> 0.2"},
      {:telemetry, "~> 1.0"},
      {:plug, ">= 1.11.0"},
      {:cowboy_telemetry, "~> 0.4", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.8", only: [:dev, :test]},
      {:opentelemetry, "~> 1.5", only: [:dev, :test]},
      {:opentelemetry_bandit, "~> 0.2.0", only: [:dev, :test]},
      {:opentelemetry_cowboy, "~> 1.0.0", only: [:dev, :test]},
      {:ex_doc, "~> 0.34", only: [:dev], runtime: false},
      {:phoenix, "~> 1.7", only: [:dev, :test]},
      {:phoenix_html, "~> 4.1", only: [:dev, :test]},
      {:plug_cowboy, "~> 2.5", only: [:dev, :test]},
      {:bandit, "~> 1.5", only: [:dev, :test]},
      {:req, "~> 0.5", only: [:dev, :test]},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false}
    ]
  end
end
