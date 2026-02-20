defmodule OpentelemetryCommanded.MixProject do
  use Mix.Project

  @version "0.2.0"
  @github_url "https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/instrumentation/opentelemetry_commanded"

  def project do
    [
      app: :opentelemetry_commanded,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps(),
      description: "Trace Commanded CQRS operations with OpenTelemetry",
      source_url: @github_url,
      homepage_url: @github_url,
      name: "Opentelemetry Commanded",
      docs: docs()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [extra_applications: [:logger]]
  end

  defp package do
    [
      licenses: ["Apache-2"],
      links: %{
        "GitHub" => @github_url,
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp deps do
    [
      {:commanded, "~> 1.4"},
      {:opentelemetry_telemetry, "~> 1.0"},
      {:telemetry, "~> 1.0"},
      {:opentelemetry, "~> 1.0"},

      # Testing
      {:jason, "~> 1.2", only: :test},
      {:ecto, "~> 3.12", only: :test},
      {:ex_doc, ">= 0.0.0", only: [:dev], runtime: false}
    ]
  end

  defp docs do
    [
      main: "OpentelemetryCommanded",
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"],
      source_url_pattern: "#{@github_url}/%{path}#L%{line}",
      extras: [
        "README.md",
        "CHANGELOG.md"
      ]
    ]
  end
end
