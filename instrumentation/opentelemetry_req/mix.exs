defmodule OpentelemetryReq.MixProject do
  use Mix.Project

  @version "0.2.0"

  def project do
    [
      app: :opentelemetry_req,
      description: description(),
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "Opentelemetry Req",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_req/%{path}#L%{line}",
        main: "OpentelemetryReq",
        extras: ["README.md"]
      ],
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_req"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp description do
    "Trace Req requests with OpenTelemetry."
  end

  defp package do
    [
      description: "OpenTelemetry tracing for Req",
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_req",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:jason, "~> 1.3"},
      {:opentelemetry_api, "~> 1.0"},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:req, ">= 0.3.5"},
      {:ex_doc, "~> 0.31", only: [:dev, :test]},
      {:opentelemetry, "~> 1.0", only: :test}
    ]
  end
end
