defmodule OpentelemetryCommanded.DummyApp.CommandValidatorMiddleware do
  @behaviour Commanded.Middleware

  alias Commanded.Middleware.Pipeline
  alias OpentelemetryCommanded.DummyApp.Commands, as: C

  def before_dispatch(%Pipeline{command: %C.DispatchError{} = command} = pipeline) do
    Pipeline.assign(pipeline, :response, {:error, command.message})
    #    Pipeline.halt(pipeline)
  end

  def before_dispatch(%Pipeline{command: _command} = pipeline) do
    pipeline
  end

  def after_dispatch(%Pipeline{command: _command} = pipeline) do
    pipeline
  end

  def after_failure(%Pipeline{command: _command} = pipeline) do
    pipeline
  end
end
