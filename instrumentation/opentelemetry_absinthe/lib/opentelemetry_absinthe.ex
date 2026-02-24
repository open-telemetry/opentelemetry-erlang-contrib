defmodule OpentelemetryAbsinthe do
  alias OpentelemetryAbsinthe.Instrumentation

  @config Instrumentation.default_config()

  # Allow alias before moduledoc
  # credo:disable-for-next-line
  @moduledoc """
  # OpentelemetryAbsinthe
  An opentelemetry instrumentation library for Absinthe

  ## Usage

  To start collecting traces just put `OpentelemetryAbsinthe.setup()` in your application start function.

  ## Configuration

  OpentelemetryAbsinthe can be configured with the application environment
  ```
  config :opentelemetry_absinthe,
    trace_options: [
      trace_request_query: false,
      trace_response_error: true,
      ...
    ]
  ```
  configuration can also be passed directly to the setup function
  ```
  OpentelemetryAbsinthe.setup(
    trace_request_query: false,
    trace_response_error: true,
    ...
  )
  ```

  ## Configuration options

    * `span_name`(default: #{Keyword.fetch!(@config, :span_name)}):

      Either
        - `:dynamic`: sets the span name dynamically, based on the operation name and type, as recommended by [opentelemetry](https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/instrumentation/graphql/). This will become the only supported option in the future.
        - `String.t()`: *deprecated* the name of the span

    * `trace_request_query`(default: #{Keyword.fetch!(@config, :trace_request_query)}): attaches the graphql query as an attribute

      **Important Note**: This is usually safe, since graphql queries are expected to be static. All dynamic data should be passed via graphql variables.
      However some libraries(for example [dillonkearns/elm-graphql](https://github.com/dillonkearns/elm-graphql/issues/27) store the variables inline as a part of the query.
      If you expect clients to send dynamic data as a part of the graphql query you should disable this.

    * `trace_request_name`(default: #{Keyword.fetch!(@config, :trace_request_name)}): attaches the graphql operation name when using batched queries as an attribute
    * `trace_request_type`(default: #{Keyword.fetch!(@config, :trace_request_type)}): attaches the graphql query type(query, mutation or subscription) as an attribute
    * `trace_request_variables`(default: #{Keyword.fetch!(@config, :trace_request_variables)}): attaches the graphql variables as an attribute
    * `trace_request_selections`(default: #{Keyword.fetch!(@config, :trace_request_selections)}): attaches the root fields queried as an attribute

      For example given a query like:
      ```
        query($isbn: String!) {
          book(isbn: $isbn) {
            title
            author {
              name
              age
            }
          }
          reader {
            name
          }
        }
      ```
      will result in a graphql.request.selections attribute with the value `["book", "reader"]` being attached.
      Note that aliased fields will use their unaliased name.

    * `trace_response_result`(default: #{Keyword.fetch!(@config, :trace_response_result)}): attaches the result returned by the server as an attribute
    * `trace_response_errors`(default: #{Keyword.fetch!(@config, :trace_response_errors)}): attaches the errors returned by the server as an attribute
    * `trace_subscriptions`(default: #{Keyword.fetch!(@config, :trace_subscriptions)}): attaches to `[:absinthe, :subscription, :publish]` (`:start` and `:stop`)

  ## Telemetry

  OpentelemetryAbsinthe exposes `telemetry` events which can be hooked into using `:telemetry.attach/4` or `:telemetry.attach_many/4`.
  The events exposed are:

  - `[:opentelemetry_absinthe, :graphql, :handled]` for when a GraphQl query has been handled, the metadata and measurements are defined in `OpentelemetryAbsinthe.Instrumentation.graphql_handled_event_metadata()` and `OpentelemetryAbsinthe.Instrumentation.graphql_handled_event_measurements()`
  """

  defdelegate setup(instrumentation_opts \\ []), to: Instrumentation
end
