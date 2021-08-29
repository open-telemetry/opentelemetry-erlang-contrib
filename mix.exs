defmodule OtelContribTests.MixProject do
  use Mix.Project

  def project do
    [
      app: :otel_contrib_tests,
      version: "0.1.0",
      deps: deps()
    ]
  end

  def deps do
    [
      # {:opentelemetry, path: "apps/opentelemetry", only: :test, override: true}
    ]
  end
end
