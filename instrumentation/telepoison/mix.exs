defmodule Telepoison.MixProject do
  use Mix.Project

  @source_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/telepoison"
  @version "1.1.2"

  def project do
    [
      app: :telepoison,
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package(),
      aliases: aliases()
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
      {:httpoison, "~> 1.6 or ~> 2.0"},
      {:opentelemetry_api, "~> 1.0"}
    ] ++ dev_deps()
  end

  def dev_deps,
    do: [
      {:opentelemetry, "~> 1.0", only: :test},
      {:opentelemetry_exporter, "~> 1.0", only: :test},
      {:plug, "~> 1.12", only: :test},
      {:plug_cowboy, "~> 2.2", only: :test},
      {:credo, "~> 1.6", only: [:dev, :test]},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false},
      {:ex_doc, ">= 0.25.3", only: :dev, runtime: false}
    ]

  defp package do
    [
      name: "telepoison",
      description: "Telepoison is a opentelemetry-instrumented wrapper around HTTPoison",
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @source_url,
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp aliases do
    [
      "format.all": [
        "format mix.exs \"lib/**/*.{ex,exs}\" \"test/**/*.{ex,exs}\" \"priv/**/*.{ex,exs}\" \"config/**/*.{ex,exs}\""
      ]
    ]
  end

  defp docs do
    [
      extras: [
        "LICENSE.md": [title: "License"],
        "README.md": [title: "Overview"]
      ],
      main: "readme",
      source_url: @source_url,
      source_ref: "v#{@version}",
      formatters: ["html"]
    ]
  end
end
