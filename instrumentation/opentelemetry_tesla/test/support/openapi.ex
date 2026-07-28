defmodule MyApi.Response do
  use Tesla.OpenAPI.Response
end

defmodule MyApi.Operation.GetItem.Path do
  alias Tesla.OpenAPI.{PathParam, PathParams, PathTemplate}

  @path_template PathTemplate.new!("/items/{id}{coords}")

  @path_params PathParams.new!([
                 PathParam.new!("id"),
                 PathParam.new!("coords", style: :matrix, explode: true)
               ])

  def path_template, do: @path_template
  def path_params, do: @path_params
end

defmodule MyApi.Operation.GetItem.Query do
  alias Tesla.OpenAPI.{QueryParam, QueryParams}

  @query_params QueryParams.new!([
                  QueryParam.new!("color", style: :pipe_delimited),
                  QueryParam.new!("filter", style: :deep_object),
                  QueryParam.new!("tags", style: :space_delimited),
                  QueryParam.new!("page")
                ])

  def query_params, do: @query_params
end

defmodule MyApi.Operation.GetItem.Header do
  alias Tesla.OpenAPI.{HeaderParam, HeaderParams}

  @header_params HeaderParams.new!([
                   HeaderParam.new!("X-Request-ID"),
                   HeaderParam.new!("X-Trace-Tag")
                 ])

  def header_params, do: @header_params
end

defmodule MyApi.Operation.GetItem.Cookie do
  alias Tesla.OpenAPI.{CookieParam, CookieParams}

  @cookie_params CookieParams.new!([
                   CookieParam.new!("session_id"),
                   CookieParam.new!("theme")
                 ])

  def cookie_params, do: @cookie_params
end

defmodule MyApi.Operation.GetItem do
  alias MyApi.Operation.GetItem.{Cookie, Header, Path, Query}
  alias Tesla.OpenAPI
  alias Tesla.OpenAPI.{CookieParams, HeaderParams, PathParams, PathTemplate, QueryParams}

  @operation_path Path.path_template().path

  @private OpenAPI.merge_private([
             PathTemplate.put_private(Path.path_template()),
             PathParams.put_private(Path.path_params()),
             QueryParams.put_private(Query.query_params())
           ])

  def operation_path, do: @operation_path
  def private, do: @private

  def handle_operation(client, opts \\ []) do
    path_values = Keyword.get(opts, :path_params, %{})
    query_values = Keyword.get(opts, :query, %{})
    header_values = Keyword.get(opts, :headers, %{})
    cookie_values = Keyword.get(opts, :cookies, %{})

    Tesla.get(client, @operation_path,
      query: query_values,
      headers: headers(header_values, cookie_values),
      opts: [path_params: path_values],
      private: @private
    )
  end

  defp headers(header_values, cookie_values) do
    HeaderParams.to_headers(Header.header_params(), header_values) ++
      CookieParams.to_headers(Cookie.cookie_params(), cookie_values)
  end
end

defmodule MyApi.Operation.CreateItem.Path do
  alias Tesla.OpenAPI.{PathParam, PathParams, PathTemplate}

  @path_template PathTemplate.new!("/tenants/{tenant_id}/items")

  @path_params PathParams.new!([
                 PathParam.new!("tenant_id")
               ])

  def path_template, do: @path_template
  def path_params, do: @path_params
end

defmodule MyApi.Operation.CreateItem.Query do
  alias Tesla.OpenAPI.{QueryParam, QueryParams}

  @query_params QueryParams.new!([
                  QueryParam.new!("dry_run"),
                  QueryParam.new!("expand", style: :form, explode: false)
                ])

  def query_params, do: @query_params
end

defmodule MyApi.Operation.CreateItem.Header do
  alias Tesla.OpenAPI.{HeaderParam, HeaderParams}

  @header_params HeaderParams.new!([
                   HeaderParam.new!("X-Request-ID"),
                   HeaderParam.new!("Idempotency-Key")
                 ])

  def header_params, do: @header_params
end

defmodule MyApi.Operation.CreateItem.Cookie do
  alias Tesla.OpenAPI.{CookieParam, CookieParams}

  @cookie_params CookieParams.new!([
                   CookieParam.new!("session_id")
                 ])

  def cookie_params, do: @cookie_params
end

defmodule MyApi.Operation.CreateItem do
  alias MyApi.Operation.CreateItem.{Cookie, Header, Path, Query}
  alias Tesla.OpenAPI
  alias Tesla.OpenAPI.{CookieParams, HeaderParams, PathParams, PathTemplate, QueryParams}

  @operation_path Path.path_template().path

  @private OpenAPI.merge_private([
             PathTemplate.put_private(Path.path_template()),
             PathParams.put_private(Path.path_params()),
             QueryParams.put_private(Query.query_params())
           ])

  def operation_path, do: @operation_path
  def private, do: @private

  def handle_operation(client, body, opts \\ []) do
    path_values = Keyword.get(opts, :path_params, %{})
    query_values = Keyword.get(opts, :query, %{})
    header_values = Keyword.get(opts, :headers, %{})
    cookie_values = Keyword.get(opts, :cookies, %{})

    Tesla.post(client, @operation_path, body,
      query: query_values,
      headers: headers(header_values, cookie_values),
      opts: [path_params: path_values],
      private: @private
    )
  end

  defp headers(header_values, cookie_values) do
    HeaderParams.to_headers(Header.header_params(), header_values) ++
      CookieParams.to_headers(Cookie.cookie_params(), cookie_values)
  end
end

defmodule MyApi.Client do
  def new(opts) do
    Tesla.client(middleware(opts))
  end

  defp middleware(opts) do
    otel_opts = Keyword.get(opts, :opentelemetry, [])

    [
      {Tesla.Middleware.BaseUrl, Keyword.fetch!(opts, :base_url)},
      Tesla.Middleware.KeepRequest,
      {Tesla.Middleware.PathParams, mode: :modern},
      {Tesla.Middleware.Query, mode: :modern},
      {Tesla.Middleware.OpenTelemetry, otel_opts}
    ]
  end
end

defmodule MyApi do
  alias MyApi.Client
  alias MyApi.Operation.{CreateItem, GetItem}

  def new(opts), do: Client.new(opts)

  def get_item(client, opts \\ []) do
    GetItem.handle_operation(client, opts)
  end

  def create_item(client, body, opts \\ []) do
    CreateItem.handle_operation(client, body, opts)
  end
end
