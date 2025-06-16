defmodule OpentelemetryXandra.MixProject do
  use Mix.Project

  @version "0.2.0"
  @description "Trace Xandra queries with OpenTelemetry."
  @repo_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib"
  @folder_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_xandra"

  def project do
    [
      app: :opentelemetry_xandra,
      description: @description,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Docs
      source_url: @folder_url,
      docs: [
        source_url_pattern: "#{@folder_url}/%{path}#L%{line}",
        main: "OpentelemetryXandra",
        extras: ["README.md"]
      ],

      # Hex
      package: [
        licenses: ["Apache-2.0"],
        links: %{
          "GitHub" => @folder_url,
          "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
          "OpenTelemetry Erlang Contrib" => @repo_url,
          "OpenTelemetry.io" => "https://opentelemetry.io"
        }
      ]
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp deps do
    [
      # Dev and test dependencies
      {:decimal, "~> 2.0", only: [:dev, :test]},
      {:ex_doc, "~> 0.38", only: :dev},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.0", only: [:dev, :test]},
      {:xandra, "~> 0.18", only: [:dev, :test]},

      # Library dependencies
      {:opentelemetry_api, "~> 1.0"},
      {:opentelemetry_process_propagator, "~> 0.3"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:telemetry, "~> 0.4 or ~> 1.0"}
    ]
  end
end
