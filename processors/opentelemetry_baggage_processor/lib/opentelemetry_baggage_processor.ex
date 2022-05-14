defmodule OpentelemetryBaggageProcessor do
  @external_resource readme = Path.join([__DIR__, "../README.md"])

  @moduledoc readme
             |> File.read!()
             |> String.split("<!-- MDOC -->")
             |> Enum.fetch!(1)
end
