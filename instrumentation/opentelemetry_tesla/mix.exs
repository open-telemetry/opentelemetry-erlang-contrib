defmodule OpentelemetryTesla.MixProject do
  use Mix.Project

  @version "2.4.0"

  def project do
    [
      app: :opentelemetry_tesla,
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: description(),
      docs: docs()
    ]
  end

  defp docs() do
    [
      main: "readme",
      extras: ["README.md"],
      source_url_pattern:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_tesla/%{path}#L%{line}"
    ]
  end

  defp description() do
    "Tesla middleware that creates OpenTelemetry spans and injects tracing headers into HTTP requests for Tesla clients."
  end

  defp package do
    [
      name: "opentelemetry_tesla",
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/instrumentation/opentelemetry_tesla",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
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
      {:opentelemetry, "~> 1.0", only: :test},
      {:opentelemetry_api, "~> 1.2"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:tesla, "~> 1.4"},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false},
      {:bypass, "~> 2.1", only: :test},
      {:jason, "~> 1.3", only: :test}
    ]
  end
end
