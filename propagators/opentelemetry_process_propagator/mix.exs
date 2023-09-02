defmodule OpentelemetryProcessPropagator.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()
    config = load_config()

    [
      app: app,
      version: to_string(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(Keyword.fetch!(config, :deps)),
      name: "Opentelemetry Process Propagator",
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/propagators/opentelemetry_process_propagator",
      docs: [
        markdown_processor: ExDoc.Markdown.Earmark,
        main: "OpentelemetryProcessPropagator",
        # logo: "path/to/logo.png",
        # erlang_docs()
        extras: []
      ],
      aliases: [
        # when build docs first build edocs with rebar3
        docs: ["cmd rebar3 edoc", "docs"]
      ],
      plt_file: {:no_warn, "priv/plts/opentelemetry_process_propagator.plt"},
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps(rebar) do
    rebar
    |> Enum.map(fn
      {dep, version} -> {dep, to_string(version)}
      dep when is_atom(dep) -> {dep, ">= 0.0.0"}
    end)
    |> Enum.concat([
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:opentelemetry, "~> 1.0", only: [:dev, :test]},
      {:opentelemetry_exporter, "~> 1.0", only: [:dev, :test]}
    ])
  end

  defp package() do
    [
      description: "Tools for opentelemetry context propagation across process boundaries",
      build_tools: ["rebar3", "mix"],
      files: ~w(lib mix.exs README.md LICENSE rebar.config rebar.lock src),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/propagators/opentelemetry_process_propagator",
        "OpenTelemetry Erlang" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry Erlang Contrib" =>
          "https://github.com/open-telemetry/opentelemetry-erlang-contrib",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  def erlang_docs() do
    files =
      for file <- Path.wildcard("edoc/*.md"),
          file != "edoc/README.md",
          do: {String.to_atom(file), [title: Path.basename(file, ".md")]}

    [{:"README.md", [title: "Overview"]} | files]
  end

  defp load_config do
    {:ok, config} = :file.consult(~c"rebar.config")

    config
  end

  defp load_app do
    {:ok, [{:application, name, desc}]} =
      :file.consult(~c"src/opentelemetry_process_propagator.app.src")

    {name, desc}
  end
end
