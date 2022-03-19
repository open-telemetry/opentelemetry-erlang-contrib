defmodule BasicElixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :basic_elixir,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [
        basic_elixir: [
          applications: [opentelemetry: :temporary]
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {BasicElixir.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:opentelemetry, "~> 1.0"},
      {:opentelemetry_exporter, "~> 1.0"}
    ]
  end
end
