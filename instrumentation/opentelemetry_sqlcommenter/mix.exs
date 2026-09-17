defmodule OpentelemetrySqlcommenter.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_sqlcommenter,
      version: "0.2.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_sqlcommenter",
      docs: [
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_sqlcommenter/%{path}#L%{line}",
        main: "OpentelemetrySqlcommenter",
        extras: ["README.md"]
      ],
      description:
        "OpenTelemetry integration for SQLCommenter and Ecto, adding telemetry context to SQL queries",
      package: [
        files: ~w(lib .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
        licenses: ["Apache-2.0"],
        links: %{
          "GitHub" =>
            "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_sqlcommenter",
          "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
          "OpenTelemetry Erlang Contrib" =>
            "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
          "OpenTelemetry.io" => "https://opentelemetry.io",
          "SQLCommenter" => "https://google.github.io/sqlcommenter/"
        }
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:opentelemetry_api, "~> 1.4"},
      {:opentelemetry, "== 1.7.0", only: [:dev, :test]},
      {:opentelemetry_exporter, "== 1.10.0", only: [:dev, :test]},
      {:ex_doc, "== 0.40.4", only: [:dev], runtime: false}
    ]
  end
end
