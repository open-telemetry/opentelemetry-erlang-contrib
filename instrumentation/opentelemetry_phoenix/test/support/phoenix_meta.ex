defmodule PhoenixMeta do
  def router_dispatch_exception(:plug_wrapper) do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123/exception",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64921},
             pid: "",
             port: 4000,
             qs: "",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 1,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"user_id" => "123"},
        path_info: ["exception"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :plug_session_fetch => fn -> :ok end
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123/exception",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdJBuZy-nj1FHgAAAaB"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      },
      error: %Plug.Conn.WrapperError{
        conn: %Plug.Conn{
          adapter:
            {Plug.Cowboy.Conn,
             %{
               bindings: %{},
               body_length: 0,
               cert: :undefined,
               has_body: false,
               headers: %{
                 "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                 "accept-encoding" => "gzip, deflate",
                 "accept-language" => "en-US,en;q=0.5",
                 "connection" => "keep-alive",
                 "host" => "localhost:4000",
                 "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
                 "tracestate" => "congo=t61rcWkgMzE",
                 "upgrade-insecure-requests" => "1",
                 "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
               },
               host: "localhost",
               host_info: :undefined,
               method: "GET",
               path: "/users/123/exception",
               path_info: :undefined,
               peer: {{10, 211, 55, 2}, 64921},
               pid: "",
               port: 4000,
               qs: "",
               ref: MyStoreWeb.Endpoint.HTTP,
               scheme: "http",
               sock: {{10, 211, 55, 2}, 4000},
               streamid: 1,
               version: :"HTTP/1.1"
             }},
          assigns: %{},
          body_params: %{},
          cookies: %{},
          halted: false,
          host: "localhost",
          method: "GET",
          owner: "",
          params: %{"page" => "1", "user_id" => "123"},
          path_info: ["users", "123", "exception"],
          path_params: %{"user_id" => "123"},
          port: 4000,
          private: %{
            MyStoreWeb.Router => {[], %{}},
            :phoenix_action => :code_exception,
            :phoenix_controller => MyStoreWeb.PageController,
            :phoenix_endpoint => MyStoreWeb.Endpoint,
            :phoenix_flash => %{},
            :phoenix_format => "html",
            :phoenix_layout => {MyStoreWeb.LayoutView, :app},
            :phoenix_request_logger => {"request_logger", "request_logger"},
            :phoenix_router => MyStoreWeb.Router,
            :phoenix_view => MyStoreWeb.PageView,
            :plug_session => %{},
            :plug_session_fetch => :done
          },
          query_params: %{"page" => "1"},
          query_string: "page=1",
          remote_ip: {10, 211, 55, 2},
          req_cookies: %{},
          req_headers: [
            {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
            {"accept-encoding", "gzip, deflate"},
            {"accept-language", "en-US,en;q=0.5"},
            {"connection", "keep-alive"},
            {"host", "localhost:4000"},
            {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
            {"tracestate", "congo=t61rcWkgMzE"},
            {"upgrade-insecure-requests", "1"},
            {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
          ],
          request_path: "/users/123/exception",
          resp_body: nil,
          resp_cookies: %{},
          resp_headers: [
            {"cache-control", "max-age=0, private, must-revalidate"},
            {"x-request-id", "FjdJBuZy-nj1FHgAAAaB"},
            {"x-frame-options", "SAMEORIGIN"},
            {"x-xss-protection", "1; mode=block"},
            {"x-content-type-options", "nosniff"},
            {"x-download-options", "noopen"},
            {"x-permitted-cross-domain-policies", "none"},
            {"cross-origin-window-policy", "deny"}
          ],
          scheme: :http,
          script_name: [],
          secret_key_base: "",
          state: :unset,
          status: nil
        },
        kind: :error,
        reason: :badarith,
        stack: [
          {MyStoreWeb.PageController, :code_exception, 2,
           [file: 'lib/my_store_web/controllers/page_controller.ex', line: 9]},
          {MyStoreWeb.PageController, :action, 2, [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
          {MyStoreWeb.PageController, :phoenix_controller_pipeline, 2,
           [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
          {Phoenix.Router, :__call__, 2, [file: 'lib/phoenix/router.ex', line: 352]},
          {MyStoreWeb.Endpoint, :plug_builder_call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
          {MyStoreWeb.Endpoint, :"call (overridable 3)", 2, [file: 'lib/plug/debugger.ex', line: 132]},
          {MyStoreWeb.Endpoint, :call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
          {Phoenix.Endpoint.Cowboy2Handler, :init, 4, [file: 'lib/phoenix/endpoint/cowboy2_handler.ex', line: 65]},
          {:cowboy_handler, :execute, 2,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_handler.erl',
             line: 37
           ]},
          {:cowboy_stream_h, :execute, 3,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
             line: 300
           ]},
          {:cowboy_stream_h, :request_process, 3,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
             line: 291
           ]},
          {:proc_lib, :init_p_do_apply, 3, [file: 'proc_lib.erl', line: 226]}
        ]
      },
      kind: :error,
      reason: %Plug.Conn.WrapperError{
        conn: %Plug.Conn{
          adapter:
            {Plug.Cowboy.Conn,
             %{
               bindings: %{},
               body_length: 0,
               cert: :undefined,
               has_body: false,
               headers: %{
                 "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                 "accept-encoding" => "gzip, deflate",
                 "accept-language" => "en-US,en;q=0.5",
                 "connection" => "keep-alive",
                 "host" => "localhost:4000",
                 "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
                 "tracestate" => "congo=t61rcWkgMzE",
                 "upgrade-insecure-requests" => "1",
                 "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
               },
               host: "localhost",
               host_info: :undefined,
               method: "GET",
               path: "/users/123/exception",
               path_info: :undefined,
               peer: {{10, 211, 55, 2}, 64291},
               pid: "",
               port: 4000,
               qs: "page=1",
               ref: MyStoreWeb.Endpoint.HTTP,
               scheme: "http",
               sock: {{10, 211, 55, 2}, 4000},
               streamid: 1,
               version: :"HTTP/1.1"
             }},
          assigns: %{},
          body_params: %{},
          cookies: %{},
          halted: false,
          host: "localhost",
          method: "GET",
          owner: "",
          params: %{"page" => "1", "user_id" => "123"},
          path_info: ["users", "123", "exception"],
          path_params: %{"user_id" => "123"},
          port: 4000,
          private: %{
            MyStoreWeb.Router => {[], %{}},
            :phoenix_action => :code_exception,
            :phoenix_controller => MyStoreWeb.PageController,
            :phoenix_endpoint => MyStoreWeb.Endpoint,
            :phoenix_flash => %{},
            :phoenix_format => "html",
            :phoenix_layout => {MyStoreWeb.LayoutView, :app},
            :phoenix_request_logger => {"request_logger", "request_logger"},
            :phoenix_router => MyStoreWeb.Router,
            :phoenix_view => MyStoreWeb.PageView,
            :plug_session => %{},
            :plug_session_fetch => :done
          },
          query_params: %{"page" => "1"},
          query_string: "page=1",
          remote_ip: {10, 211, 55, 2},
          req_cookies: %{},
          req_headers: [
            {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
            {"accept-encoding", "gzip, deflate"},
            {"accept-language", "en-US,en;q=0.5"},
            {"connection", "keep-alive"},
            {"host", "localhost:4000"},
            {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
            {"tracestate", "congo=t61rcWkgMzE"},
            {"upgrade-insecure-requests", "1"},
            {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
          ],
          request_path: "/users/123/exception",
          resp_body: nil,
          resp_cookies: %{},
          resp_headers: [
            {"cache-control", "max-age=0, private, must-revalidate"},
            {"x-request-id", "FjdJBuZy-nj1FHgAAAaB"},
            {"x-frame-options", "SAMEORIGIN"},
            {"x-xss-protection", "1; mode=block"},
            {"x-content-type-options", "nosniff"},
            {"x-download-options", "noopen"},
            {"x-permitted-cross-domain-policies", "none"},
            {"cross-origin-window-policy", "deny"}
          ],
          scheme: :http,
          script_name: [],
          secret_key_base: "",
          state: :unset,
          status: nil
        },
        kind: :error,
        reason: :badarith,
        stack: [
          {MyStoreWeb.PageController, :code_exception, 2,
           [file: 'lib/my_store_web/controllers/page_controller.ex', line: 9]},
          {MyStoreWeb.PageController, :action, 2, [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
          {MyStoreWeb.PageController, :phoenix_controller_pipeline, 2,
           [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
          {Phoenix.Router, :__call__, 2, [file: 'lib/phoenix/router.ex', line: 352]},
          {MyStoreWeb.Endpoint, :plug_builder_call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
          {MyStoreWeb.Endpoint, :"call (overridable 3)", 2, [file: 'lib/plug/debugger.ex', line: 132]},
          {MyStoreWeb.Endpoint, :call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
          {Phoenix.Endpoint.Cowboy2Handler, :init, 4, [file: 'lib/phoenix/endpoint/cowboy2_handler.ex', line: 65]},
          {:cowboy_handler, :execute, 2,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_handler.erl',
             line: 37
           ]},
          {:cowboy_stream_h, :execute, 3,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
             line: 300
           ]},
          {:cowboy_stream_h, :request_process, 3,
           [
             file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
             line: 291
           ]},
          {:proc_lib, :init_p_do_apply, 3, [file: 'proc_lib.erl', line: 226]}
        ]
      },
      stacktrace: [
        {MyStoreWeb.PageController, :code_exception, 2,
         [file: 'lib/my_store_web/controllers/page_controller.ex', line: 9]},
        {MyStoreWeb.PageController, :action, 2, [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
        {MyStoreWeb.PageController, :phoenix_controller_pipeline, 2,
         [file: 'lib/my_store_web/controllers/page_controller.ex', line: 1]},
        {Phoenix.Router, :__call__, 2, [file: 'lib/phoenix/router.ex', line: 352]},
        {MyStoreWeb.Endpoint, :plug_builder_call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
        {MyStoreWeb.Endpoint, :"call (overridable 3)", 2, [file: 'lib/plug/debugger.ex', line: 132]},
        {MyStoreWeb.Endpoint, :call, 2, [file: 'lib/my_store_web/endpoint.ex', line: 1]},
        {Phoenix.Endpoint.Cowboy2Handler, :init, 4, [file: 'lib/phoenix/endpoint/cowboy2_handler.ex', line: 65]},
        {:cowboy_handler, :execute, 2,
         [
           file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_handler.erl',
           line: 37
         ]},
        {:cowboy_stream_h, :execute, 3,
         [
           file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
           line: 300
         ]},
        {:cowboy_stream_h, :request_process, 3,
         [
           file: '/Users/bryan/dev/opentelemetry_phoenix/test/support/my_store/deps/cowboy/src/cowboy_stream_h.erl',
           line: 291
         ]},
        {:proc_lib, :init_p_do_apply, 3, [file: 'proc_lib.erl', line: 226]}
      ]
    }
  end

  def router_dispatch_exception(:normal) do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123/exception",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64921},
             pid: "",
             port: 4000,
             qs: "",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 1,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"user_id" => "123"},
        path_info: ["exception"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :plug_session_fetch => fn -> :ok end
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123/exception",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdJBuZy-nj1FHgAAAaB"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      },
      kind: :error,
      reason: {
        :badkey,
        :name,
        %{
          username: "rick"
        }
      },
      stacktrace: [
        {MyStore.Users, :sort_by_name, 2, [file: 'lib/my_store/users.ex', line: 159]},
        {Enum, :"-to_sort_fun/1-fun-0-", 3, [file: 'lib/enum.ex', line: 2542]},
        {:lists, :sort, 2, [file: 'lists.erl', line: 969]}
      ]
    }
  end

  def endpoint_stop do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 2,
             version: :"HTTP/1.1"
           }},
        assigns: %{layout: {MyStoreWeb.LayoutView, "app.html"}},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1", "user_id" => "123"},
        path_info: ["users", "123"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_action => :user,
          :phoenix_controller => MyStoreWeb.PageController,
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_flash => %{},
          :phoenix_format => "html",
          :phoenix_layout => {MyStoreWeb.LayoutView, :app},
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :phoenix_template => "index.html",
          :phoenix_view => MyStoreWeb.PageView,
          :plug_session => %{},
          :plug_session_fetch => :done
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123",
        resp_body: [
          "<!DOCTYPE html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"utf-8\"/>\n    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"/>\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"/>\n    <title>MyStore · Phoenix Framework</title>\n    <link rel=\"stylesheet\" href=\"",
          "/css/app.css",
          "\"/>\n    <script defer type=\"text/javascript\" src=\"",
          "/js/app.js",
          "\"></script>\n  </head>\n  <body>\n    <header>",
          "    </main>\n  </body>\n</html>\n"
        ],
        resp_cookies: %{},
        resp_headers: [
          {"content-type", "text/html; charset=utf-8"},
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdyKN4aQSWR3BMAAAAI"},
          {"x-frame-options", "SAMEORIGIN"},
          {"x-xss-protection", "1; mode=block"},
          {"x-content-type-options", "nosniff"},
          {"x-download-options", "noopen"},
          {"x-permitted-cross-domain-policies", "none"},
          {"cross-origin-window-policy", "deny"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :set,
        status: 200
      }
    }
  end

  def endpoint_stop(:exception) do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123/exception",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 1,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1", "user_id" => "123"},
        path_info: ["users", "123", "exception"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_action => :code_exception,
          :phoenix_controller => MyStoreWeb.PageController,
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_flash => %{},
          :phoenix_format => "html",
          :phoenix_layout => {MyStoreWeb.LayoutView, :app},
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :phoenix_view => MyStoreWeb.PageView,
          :plug_session => %{},
          :plug_session_fetch => :done
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123/exception",
        resp_body:
          "<!DOCTYPE html>\n<html>\n<head>\n    <meta charset=\"utf-8\">\n    <title>ArithmeticError at GET /users/123/exception</title>\n    <meta name=\"viewport\" content=\"width=device-width\">\n...",
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdxbmwZYwjZpIQAAAAJ"},
          {"x-frame-options", "SAMEORIGIN"},
          {"x-xss-protection", "1; mode=block"},
          {"x-content-type-options", "nosniff"},
          {"x-download-options", "noopen"},
          {"x-permitted-cross-domain-policies", "none"},
          {"cross-origin-window-policy", "deny"},
          {"content-type", "text/html; charset=utf-8"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :set,
        status: 500
      }
    }
  end

  def endpoint_start do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 2,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %Plug.Conn.Unfetched{aspect: :body_params},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1"},
        path_info: ["users", "123"],
        path_params: %{},
        port: 4000,
        private: %{
          phoenix_endpoint: MyStoreWeb.Endpoint,
          phoenix_request_logger: {"request_logger", "request_logger"}
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdyKN4aQSWR3BMAAAAI"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      }
    }
  end

  def endpoint_start(:exception) do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123/exception",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 1,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %Plug.Conn.Unfetched{aspect: :body_params},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1"},
        path_info: ["users", "123", "exception"],
        path_params: %{},
        port: 4000,
        private: %{
          phoenix_endpoint: MyStoreWeb.Endpoint,
          phoenix_request_logger: {"request_logger", "request_logger"}
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123/exception",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdxbmwZYwjZpIQAAAAJ"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      }
    }
  end

  def router_dispatch_start do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 2,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1", "user_id" => "123"},
        path_info: ["users", "123"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :plug_session_fetch => fn -> :ok end
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdyKN4aQSWR3BMAAAAI"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      },
      log: :debug,
      path_params: %{"user_id" => "123"},
      pipe_through: [:browser],
      plug: MyStoreWeb.PageController,
      plug_opts: :user,
      route: "/users/:user_id"
    }
  end

  def router_dispatch_start(:exception) do
    %{
      conn: %Plug.Conn{
        adapter:
          {Plug.Cowboy.Conn,
           %{
             bindings: %{},
             body_length: 0,
             cert: :undefined,
             has_body: false,
             headers: %{
               "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
               "accept-encoding" => "gzip, deflate",
               "accept-language" => "en-US,en;q=0.5",
               "cache-control" => "max-age=0",
               "connection" => "keep-alive",
               "host" => "localhost:4000",
               "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
               "tracestate" => "congo=t61rcWkgMzE",
               "upgrade-insecure-requests" => "1",
               "user-agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"
             },
             host: "localhost",
             host_info: :undefined,
             method: "GET",
             path: "/users/123/exception",
             path_info: :undefined,
             peer: {{10, 211, 55, 2}, 64291},
             pid: "",
             port: 4000,
             qs: "page=1",
             ref: MyStoreWeb.Endpoint.HTTP,
             scheme: "http",
             sock: {{10, 211, 55, 2}, 4000},
             streamid: 1,
             version: :"HTTP/1.1"
           }},
        assigns: %{},
        body_params: %{},
        cookies: %{},
        halted: false,
        host: "localhost",
        method: "GET",
        owner: "",
        params: %{"page" => "1", "user_id" => "123"},
        path_info: ["users", "123", "exception"],
        path_params: %{"user_id" => "123"},
        port: 4000,
        private: %{
          MyStoreWeb.Router => {[], %{}},
          :phoenix_endpoint => MyStoreWeb.Endpoint,
          :phoenix_request_logger => {"request_logger", "request_logger"},
          :phoenix_router => MyStoreWeb.Router,
          :plug_session_fetch => fn -> :ok end
        },
        query_params: %{"page" => "1"},
        query_string: "page=1",
        remote_ip: {10, 211, 55, 2},
        req_cookies: %{},
        req_headers: [
          {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          {"accept-encoding", "gzip, deflate"},
          {"accept-language", "en-US,en;q=0.5"},
          {"cache-control", "max-age=0"},
          {"connection", "keep-alive"},
          {"host", "localhost:4000"},
          {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
          {"tracestate", "congo=t61rcWkgMzE"},
          {"upgrade-insecure-requests", "1"},
          {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}
        ],
        request_path: "/users/123/exception",
        resp_body: nil,
        resp_cookies: %{},
        resp_headers: [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"x-request-id", "FjdxbmwZYwjZpIQAAAAJ"}
        ],
        scheme: :http,
        script_name: [],
        secret_key_base: "",
        state: :unset,
        status: nil
      },
      log: :debug,
      path_params: %{"user_id" => "123"},
      pipe_through: [:browser],
      plug: MyStoreWeb.PageController,
      plug_opts: :code_exception,
      route: "/users/:user_id/exception"
    }
  end
end
