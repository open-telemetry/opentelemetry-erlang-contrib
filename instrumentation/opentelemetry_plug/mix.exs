defmodule OpentelemetryPlug.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_plug,
      description: description(),
      version: "1.0.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_plug",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_plug/%{path}#L%{line}",
        main: "OpentelemetryPlug",
        extras: ["README.md"]
      ]
    ]
  end

  defp description do
    "Trace Plug Routes with OpenTelemetry."
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_plug",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:hackney, "~> 1.0", only: :test, runtime: false},
      {:opentelemetry_api, "~> 1.0"},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:plug, ">= 1.10.1"},
      {:plug_cowboy, "~> 2.2", only: :test, runtime: false},
      {:telemetry, "~> 0.4 or ~> 1.0"}
    ]
  end
end
