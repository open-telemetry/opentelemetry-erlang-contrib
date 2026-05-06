defmodule OpentelemetryTesla.MixProject do
  use Mix.Project

  @app :opentelemetry_tesla
  @version "3.0.0"
  @source_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib"
  @tag "opentelemetry-tesla-v#{@version}"

  def project do
    [
      app: @app,
      description: description(),
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      name: "Opentelemetry Tesla",
      docs: docs(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: "#{@source_url}/tree/#{@tag}/instrumentation/#{@app}"
    ]
  end

  defp docs do
    [
      source_url_pattern: "#{@source_url}/blob/#{@tag}/instrumentation/#{@app}/%{path}#L%{line}",
      main: "readme",
      extras: ["README.md"]
    ]
  end

  defp description do
    "Tesla middleware that creates OpenTelemetry spans and injects tracing headers into HTTP requests for Tesla clients."
  end

  defp package do
    [
      name: "#{@app}",
      description: "OpenTelemetry tracing for Tesla HTTP client",
      files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "#{@source_url}/tree/#{@tag}/instrumentation/#{@app}",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" => @source_url,
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

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nimble_options, "~> 1.1"},
      {:opentelemetry, "~> 1.0", only: :test},
      {:opentelemetry_api, "~> 1.2"},
      {:opentelemetry_telemetry, "~> 1.1"},
      {:opentelemetry_semantic_conventions, "~> 1.27"},
      {:tesla, "~> 1.4"},
      {:otel_http, "~> 0.2"},
      {:ex_doc, "~> 0.40", only: :dev, runtime: false},
      {:bypass, "~> 2.1", only: :test},
      {:jason, "~> 1.3", only: :test},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false}
    ]
  end
end
