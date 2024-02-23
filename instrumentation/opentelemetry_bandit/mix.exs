defmodule OpentelemetryBandit.MixProject do
  use Mix.Project

  @version "0.1.4"

  def project do
    [
      app: :opentelemetry_bandit,
      version: @version,
      elixir: "~> 1.14",
      aliases: aliases(),
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      elixirc_paths: elixirc_path(Mix.env()),
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.cobertura": :test
      ],
      docs: [
        main: "readme",
        extras: ["README.md"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp description do
    """
    Telemetry handler that creates Opentelemetry spans from Bandit events.
    """
  end

  defp package do
    [
      files: ~w(lib .formatter.exs mix.exs LICENSE* README* CHANGELOG*),
      maintainers: ["Artem Solomatin"],
      licenses: ["Apache-2.0"],

      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_bandit",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp elixirc_path(:test), do: ["lib/", "test/support"]
  defp elixirc_path(_), do: ["lib/"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:opentelemetry_api, "~> 1.2"},
      {:opentelemetry_semantic_conventions, "~> 0.2"},
      {:opentelemetry_telemetry, "~> 1.0"},
      {:plug, ">= 1.15.0"},
      {:telemetry, "~> 1.2"},

      # dev dependencies
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:bandit, "~> 1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.0", only: [:dev, :test]},
      {:req, "~> 0.4", only: [:dev, :test]}
    ]
  end

  defp aliases do
    ["test.coverage": ["coveralls.cobertura"]]
  end
end
