defmodule OpentelemetryAbsinthe.MixProject do
  use Mix.Project

  @source_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_absinthe"
  @version "2.3.0"

  def project do
    [
      app: :opentelemetry_absinthe,
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package(),
      aliases: aliases()
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp docs do
    [
      main: "OpentelemetryAbsinthe",
      source_ref: @version,
      source_url: @source_url,
      formatters: ["html"],
      extras: ["README.md"]
    ]
  end

  defp deps do
    [
      {:absinthe, "~> 1.7"},
      {:jason, "~> 1.2"},
      {:opentelemetry_api, "~> 1.1"},
      {:telemetry, "~> 0.4 or ~> 1.0"}
    ] ++ dev_deps()
  end

  defp dev_deps do
    [
      {:opentelemetry, "~> 1.1", only: :test},
      {:opentelemetry_exporter, "~> 1.1", only: :test},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:credo, "~> 1.4", only: [:dev, :test]},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end

  defp aliases do
    [
      "format.all": [
        "format mix.exs \"lib/**/*.{ex,exs}\" \"test/**/*.{ex,exs}\" \"priv/**/*.{ex,exs}\" \"config/**/*.{ex,exs}\""
      ]
    ]
  end

  def package do
    [
      name: "opentelemetry_absinthe",
      description:
        "OpentelemetryAbsinthe is a opentelemetry-instrumented wrapper around Absinthe",
      licenses: ["Apache-2.0"],
      links: %{
        "Github" => @source_url,
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end
end
