defmodule OpentelemetrySqlcommenter.MixProject do
  use Mix.Project

  def project do
    [
      app: :opentelemetry_sqlcommenter,
      version: "0.1.1",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description:
        "OpenTelemetry integration for SQLCommenter and Ecto, adding telemetry context to SQL queries",
      package: [
        licenses: ["Apache-2.0"],
        links: %{
          "GitHub" => "https://github.com/dkuku/opentelemetry_sqlcommenter",
          "OpenTelemetry" => "https://opentelemetry.io",
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
      {:opentelemetry_exporter, "~> 1.8"},
      {:opentelemetry, "~> 1.0"},
      {:postgrex, "~> 0.19.3"},
      {:ex_doc, "~> 0.35"}
    ]
  end
end
