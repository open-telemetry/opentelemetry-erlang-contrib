defmodule PhoenixLiveViewMeta do
  def mount_start() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          connect_info: %{session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}},
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{}, []},
          connect_params: %{
            "_csrf_token" => "WicVN2INLT9RI2QiXQY1LwAhFGNvIiAA3FFC3wzH0o1C1IOHSgBP-opG",
            "_mounts" => 5,
            "_track_static" => [
              "http://localhost:4000/assets/app.css",
              "http://localhost:4000/assets/app.js"
            ]
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"},
      session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
    }
  end

  def mount_start(:exception) do
    %{
      socket: %{
        id: "phx-F5N4UNBnc7walAAU",
        private: %{
          connect_info: %{
            owner: IEx.Helpers.pid(0, 688, 0),
            port: 4000,
            private: %{
              :phoenix_live_view =>
                {NnnnnWeb.MyTestLive, [action: :index, router: NnnnnWeb.Router],
                 %{extra: %{}, name: :default, vsn: 1_698_660_624_598_806_627}},
              NnnnnWeb.Router => [],
              :phoenix_endpoint => NnnnnWeb.Endpoint,
              :plug_session_fetch => :done,
              :plug_session => %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"},
              :before_send => [],
              :phoenix_router => NnnnnWeb.Router,
              :phoenix_request_logger => {"request_logger", "request_logger"},
              :phoenix_layout => %{_: false},
              :phoenix_format => "html",
              :phoenix_root_layout => %{"html" => {NnnnnWeb.Layouts, :root}}
            },
            scheme: :http,
            status: nil,
            script_name: [],
            state: :unset,
            host: "localhost",
            params: %{"foo" => "bar"},
            __struct__: Plug.Conn,
            halted: false,
            adapter:
              {Plug.Cowboy.Conn,
               %{
                 pid: IEx.Helpers.pid(0, 540, 0),
                 port: 4000,
                 scheme: "http",
                 version: :"HTTP/1.1",
                 path: "/live",
                 host: "localhost",
                 peer: {{127, 0, 0, 1}, 56258},
                 bindings: %{},
                 ref: NnnnnWeb.Endpoint.HTTP,
                 cert: :undefined,
                 headers: %{
                   "accept" => "text/html,
                application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                   "accept-encoding" => "gzip, deflate, br",
                   "accept-language" => "en-US,en;q=0.5",
                   "connection" => "keep-alive",
                   "cookie" =>
                     "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
                   "dnt" => "1",
                   "host" => "localhost:4000",
                   "referer" => "http://localhost:4000/live?foo=bar",
                   "sec-fetch-dest" => "document",
                   "sec-fetch-mode" => "navigate",
                   "sec-fetch-site" => "same-origin",
                   "upgrade-insecure-requests" => "1",
                   "user-agent" => "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"
                 },
                 method: "GET",
                 path_info: :undefined,
                 host_info: :undefined,
                 streamid: 7,
                 body_length: 0,
                 has_body: false,
                 qs: "foo=bar",
                 sock: {{127, 0, 0, 1}, 4000}
               }},
            secret_key_base: "r3goJWs3YJg7L+ErgaLFRiHb/eKTnziWV9uvCsGnqVERvt+iHmi/hE+KNaQdxeBA",
            cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            },
            request_path: "/live",
            method: "GET",
            assigns: %{flash: %{}},
            remote_ip: {127, 0, 0, 1},
            req_headers: [
              {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"},
              {"accept-encoding", "gzip, deflate, br"},
              {"accept-language", "en-US,en;q=0.5"},
              {"connection", "keep-alive"},
              {"cookie",
               "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE"},
              {"dnt", "1"},
              {"host", "localhost:4000"},
              {"referer", "http://localhost:4000/live?foo=bar"},
              {"sec-fetch-dest", "document"},
              {"sec-fetch-mode", "navigate"},
              {"sec-fetch-site", "same-origin"},
              {"upgrade-insecure-requests", "1"},
              {"user-agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"}
            ],
            path_info: ["live"],
            resp_headers: [
              {"cache-control", "max-age=0, private, must-revalidate"},
              {"x-request-id", "F5N4UNBMNggiGpQAAAkE"},
              {"referrer-policy", "strict-origin-when-cross-origin"},
              {"x-content-type-options", "nosniff"},
              {"x-download-options", "noopen"},
              {"x-frame-options", "SAMEORIGIN"},
              {"x-permitted-cross-domain-policies", "none"}
            ],
            resp_cookies: %{},
            resp_body: nil,
            body_params: %{},
            path_params: %{},
            query_params: %{"foo" => "bar"},
            query_string: "foo=bar",
            req_cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            }
          },
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{flash: %{}}, []},
          connect_params: %{},
          root_view: NnnnnWeb.MyTestLive,
          conn_session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: nil,
        root_pid: nil,
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"},
      session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
    }
  end

  def mount_stop() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          connect_info: %{session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}},
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{}, []},
          connect_params: %{
            "_csrf_token" => "WicVN2INLT9RI2QiXQY1LwAhFGNvIiAA3FFC3wzH0o1C1IOHSgBP-opG",
            "_mounts" => 5,
            "_track_static" => [
              "http://localhost:4000/assets/app.css",
              "http://localhost:4000/assets/app.js"
            ]
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"},
      session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
    }
  end

  def mount_stop(:exception) do
    %{
      socket: %{
        id: "phx-F5N4UNBnc7walAAU",
        private: %{
          connect_info: %{
            owner: IEx.Helpers.pid(0, 540, 0),
            port: 4000,
            private: %{
              :phoenix_live_view =>
                {NnnnnWeb.MyTestLive, [action: :index, router: NnnnnWeb.Router],
                 %{extra: %{}, name: :default, vsn: 1_698_660_624_598_806_627}},
              NnnnnWeb.Router => [],
              :phoenix_endpoint => NnnnnWeb.Endpoint,
              :plug_session_fetch => :done,
              :plug_session => %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"},
              :before_send => [],
              :phoenix_router => NnnnnWeb.Router,
              :phoenix_request_logger => {"request_logger", "request_logger"},
              :phoenix_layout => %{_: false},
              :phoenix_format => "html",
              :phoenix_root_layout => %{"html" => {NnnnnWeb.Layouts, :root}}
            },
            scheme: :http,
            status: nil,
            script_name: [],
            state: :unset,
            host: "localhost",
            params: %{"foo" => "bar"},
            __struct__: Plug.Conn,
            halted: false,
            adapter:
              {Plug.Cowboy.Conn,
               %{
                 pid: IEx.Helpers.pid(0, 540, 0),
                 port: 4000,
                 scheme: "http",
                 version: :"HTTP/1.1",
                 path: "/live",
                 host: "localhost",
                 peer: {{127, 0, 0, 1}, 56258},
                 bindings: %{},
                 ref: NnnnnWeb.Endpoint.HTTP,
                 cert: :undefined,
                 headers: %{
                   "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                   "accept-encoding" => "gzip, deflate, br",
                   "accept-language" => "en-US,en;q=0.5",
                   "connection" => "keep-alive",
                   "cookie" =>
                     "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
                   "dnt" => "1",
                   "host" => "localhost:4000",
                   "referer" => "http://localhost:4000/live?foo=bar",
                   "sec-fetch-dest" => "document",
                   "sec-fetch-mode" => "navigate",
                   "sec-fetch-site" => "same-origin",
                   "upgrade-insecure-requests" => "1",
                   "user-agent" => "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"
                 },
                 method: "GET",
                 path_info: :undefined,
                 host_info: :undefined,
                 streamid: 7,
                 body_length: 0,
                 has_body: false,
                 qs: "foo=bar",
                 sock: {{127, 0, 0, 1}, 4000}
               }},
            secret_key_base: "r3goJWs3YJg7L+ErgaLFRiHb/eKTnziWV9uvCsGnqVERvt+iHmi/hE+KNaQdxeBA",
            cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            },
            request_path: "/live",
            method: "GET",
            assigns: %{flash: %{}},
            remote_ip: {127, 0, 0, 1},
            req_headers: [
              {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"},
              {"accept-encoding", "gzip, deflate, br"},
              {"accept-language", "en-US,en;q=0.5"},
              {"connection", "keep-alive"},
              {"cookie",
               "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE"},
              {"dnt", "1"},
              {"host", "localhost:4000"},
              {"referer", "http://localhost:4000/live?foo=bar"},
              {"sec-fetch-dest", "document"},
              {"sec-fetch-mode", "navigate"},
              {"sec-fetch-site", "same-origin"},
              {"upgrade-insecure-requests", "1"},
              {"user-agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"}
            ],
            path_info: ["live"],
            resp_headers: [
              {"cache-control", "max-age=0, private, must-revalidate"},
              {"x-request-id", "F5N4UNBMNggiGpQAAAkE"},
              {"referrer-policy", "strict-origin-when-cross-origin"},
              {"x-content-type-options", "nosniff"},
              {"x-download-options", "noopen"},
              {"x-frame-options", "SAMEORIGIN"},
              {"x-permitted-cross-domain-policies", "none"}
            ],
            resp_cookies: %{},
            resp_body: nil,
            body_params: %{},
            path_params: %{},
            query_params: %{"foo" => "bar"},
            query_string: "foo=bar",
            req_cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            }
          },
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{flash: %{}}, []},
          connect_params: %{},
          root_view: NnnnnWeb.MyTestLive,
          conn_session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: nil,
        root_pid: nil,
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"},
      session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
    }
  end

  def handle_params_start() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          connect_info: %{session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}},
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{}, []},
          connect_params: %{
            "_csrf_token" => "WicVN2INLT9RI2QiXQY1LwAhFGNvIiAA3FFC3wzH0o1C1IOHSgBP-opG",
            "_mounts" => 5,
            "_track_static" => [
              "http://localhost:4000/assets/app.css",
              "http://localhost:4000/assets/app.js"
            ]
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"}
    }
  end

  def handle_params_start(:exception) do
    %{
      socket: %{
        id: "phx-F5N4UNBnc7walAAU",
        private: %{
          connect_info: %{
            owner: IEx.Helpers.pid(0, 540, 0),
            port: 4000,
            private: %{
              :phoenix_live_view =>
                {NnnnnWeb.MyTestLive, [action: :index, router: NnnnnWeb.Router],
                 %{extra: %{}, name: :default, vsn: 1_698_660_624_598_806_627}},
              NnnnnWeb.Router => [],
              :phoenix_endpoint => NnnnnWeb.Endpoint,
              :plug_session_fetch => :done,
              :plug_session => %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"},
              :before_send => [],
              :phoenix_router => NnnnnWeb.Router,
              :phoenix_request_logger => {"request_logger", "request_logger"},
              :phoenix_layout => %{_: false},
              :phoenix_format => "html",
              :phoenix_root_layout => %{"html" => {NnnnnWeb.Layouts, :root}}
            },
            scheme: :http,
            status: nil,
            script_name: [],
            state: :unset,
            host: "localhost",
            params: %{"foo" => "bar"},
            __struct__: Plug.Conn,
            halted: false,
            adapter:
              {Plug.Cowboy.Conn,
               %{
                 pid: IEx.Helpers.pid(0, 540, 0),
                 port: 4000,
                 scheme: "http",
                 version: :"HTTP/1.1",
                 path: "/live",
                 host: "localhost",
                 peer: {{127, 0, 0, 1}, 56258},
                 bindings: %{},
                 ref: NnnnnWeb.Endpoint.HTTP,
                 cert: :undefined,
                 headers: %{
                   "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                   "accept-encoding" => "gzip, deflate, br",
                   "accept-language" => "en-US,en;q=0.5",
                   "connection" => "keep-alive",
                   "cookie" =>
                     "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
                   "dnt" => "1",
                   "host" => "localhost:4000",
                   "referer" => "http://localhost:4000/live?foo=bar",
                   "sec-fetch-dest" => "document",
                   "sec-fetch-mode" => "navigate",
                   "sec-fetch-site" => "same-origin",
                   "upgrade-insecure-requests" => "1",
                   "user-agent" => "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"
                 },
                 method: "GET",
                 path_info: :undefined,
                 host_info: :undefined,
                 streamid: 7,
                 body_length: 0,
                 has_body: false,
                 qs: "foo=bar",
                 sock: {{127, 0, 0, 1}, 4000}
               }},
            secret_key_base: "r3goJWs3YJg7L+ErgaLFRiHb/eKTnziWV9uvCsGnqVERvt+iHmi/hE+KNaQdxeBA",
            cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            },
            request_path: "/live",
            method: "GET",
            assigns: %{flash: %{}},
            remote_ip: {127, 0, 0, 1},
            req_headers: [
              {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"},
              {"accept-encoding", "gzip, deflate, br"},
              {"accept-language", "en-US,en;q=0.5"},
              {"connection", "keep-alive"},
              {"cookie",
               "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE"},
              {"dnt", "1"},
              {"host", "localhost:4000"},
              {"referer", "http://localhost:4000/live?foo=bar"},
              {"sec-fetch-dest", "document"},
              {"sec-fetch-mode", "navigate"},
              {"sec-fetch-site", "same-origin"},
              {"upgrade-insecure-requests", "1"},
              {"user-agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"}
            ],
            path_info: ["live"],
            resp_headers: [
              {"cache-control", "max-age=0, private, must-revalidate"},
              {"x-request-id", "F5N4UNBMNggiGpQAAAkE"},
              {"referrer-policy", "strict-origin-when-cross-origin"},
              {"x-content-type-options", "nosniff"},
              {"x-download-options", "noopen"},
              {"x-frame-options", "SAMEORIGIN"},
              {"x-permitted-cross-domain-policies", "none"}
            ],
            resp_cookies: %{},
            resp_body: nil,
            body_params: %{},
            path_params: %{},
            query_params: %{"foo" => "bar"},
            query_string: "foo=bar",
            req_cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            }
          },
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{flash: %{}}, []},
          connect_params: %{},
          root_view: NnnnnWeb.MyTestLive,
          conn_session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: nil,
        root_pid: nil,
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"}
    }
  end

  def handle_params_exception(:exception) do
    %{
      reason: %{message: "stop", __struct__: RuntimeError, __exception__: true},
      socket: %{
        id: "phx-F5N4UNBnc7walAAU",
        private: %{
          connect_info: %{
            owner: IEx.Helpers.pid(0, 540, 0),
            port: 4000,
            private: %{
              :phoenix_live_view =>
                {NnnnnWeb.MyTestLive, [action: :index, router: NnnnnWeb.Router],
                 %{extra: %{}, name: :default, vsn: 1_698_660_624_598_806_627}},
              NnnnnWeb.Router => [],
              :phoenix_endpoint => NnnnnWeb.Endpoint,
              :plug_session_fetch => :done,
              :plug_session => %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"},
              :before_send => [],
              :phoenix_router => NnnnnWeb.Router,
              :phoenix_request_logger => {"request_logger", "request_logger"},
              :phoenix_layout => %{_: false},
              :phoenix_format => "html",
              :phoenix_root_layout => %{"html" => {NnnnnWeb.Layouts, :root}}
            },
            scheme: :http,
            status: nil,
            script_name: [],
            state: :unset,
            host: "localhost",
            params: %{"foo" => "bar"},
            __struct__: Plug.Conn,
            halted: false,
            adapter:
              {Plug.Cowboy.Conn,
               %{
                 pid: IEx.Helpers.pid(0, 540, 0),
                 port: 4000,
                 scheme: "http",
                 version: :"HTTP/1.1",
                 path: "/live",
                 host: "localhost",
                 peer: {{127, 0, 0, 1}, 56258},
                 bindings: %{},
                 ref: NnnnnWeb.Endpoint.HTTP,
                 cert: :undefined,
                 headers: %{
                   "accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                   "accept-encoding" => "gzip, deflate, br",
                   "accept-language" => "en-US,en;q=0.5",
                   "connection" => "keep-alive",
                   "cookie" =>
                     "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
                   "dnt" => "1",
                   "host" => "localhost:4000",
                   "referer" => "http://localhost:4000/live?foo=bar",
                   "sec-fetch-dest" => "document",
                   "sec-fetch-mode" => "navigate",
                   "sec-fetch-site" => "same-origin",
                   "upgrade-insecure-requests" => "1",
                   "user-agent" => "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"
                 },
                 method: "GET",
                 path_info: :undefined,
                 host_info: :undefined,
                 streamid: 7,
                 body_length: 0,
                 has_body: false,
                 qs: "foo=bar",
                 sock: {{127, 0, 0, 1}, 4000}
               }},
            secret_key_base: "r3goJWs3YJg7L+ErgaLFRiHb/eKTnziWV9uvCsGnqVERvt+iHmi/hE+KNaQdxeBA",
            cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            },
            request_path: "/live",
            method: "GET",
            assigns: %{flash: %{}},
            remote_ip: {127, 0, 0, 1},
            req_headers: [
              {"accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"},
              {"accept-encoding", "gzip, deflate, br"},
              {"accept-language", "en-US,en;q=0.5"},
              {"connection", "keep-alive"},
              {"cookie",
               "_lv_for_bug_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg; _new_phx_thing_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI; _code_code_ship_session=SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw; _post_checkout_survey_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs; lb:session=SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4; lb:probe_cookie=; TawkConnectionTime=0; twk_idm_key=cYY3tqqSg04zLnS6YawTb; _nnnnn_key=SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE"},
              {"dnt", "1"},
              {"host", "localhost:4000"},
              {"referer", "http://localhost:4000/live?foo=bar"},
              {"sec-fetch-dest", "document"},
              {"sec-fetch-mode", "navigate"},
              {"sec-fetch-site", "same-origin"},
              {"upgrade-insecure-requests", "1"},
              {"user-agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/119.0"}
            ],
            path_info: ["live"],
            resp_headers: [
              {"cache-control", "max-age=0, private, must-revalidate"},
              {"x-request-id", "F5N4UNBMNggiGpQAAAkE"},
              {"referrer-policy", "strict-origin-when-cross-origin"},
              {"x-content-type-options", "nosniff"},
              {"x-download-options", "noopen"},
              {"x-frame-options", "SAMEORIGIN"},
              {"x-permitted-cross-domain-policies", "none"}
            ],
            resp_cookies: %{},
            resp_body: nil,
            body_params: %{},
            path_params: %{},
            query_params: %{"foo" => "bar"},
            query_string: "foo=bar",
            req_cookies: %{
              "TawkConnectionTime" => "0",
              "_code_code_ship_session" =>
                "SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYdURLR0trQ1hMTWtGSGVpdVE0NWlJazJ4bQAAAA5saXZlX3NvY2tldF9pZG0AAAA7dXNlcnNfc2Vzc2lvbnM6cFJLVzhrRE5FeV81QXBHcUhRclFiVm5rYmhNMEVqZjBJZlJaZTZuVXVIVT1tAAAACnVzZXJfdG9rZW5tAAAAIKUSlvJAzRMv-QKRqh0K0G1Z5G4TNBI39CH0WXup1Lh1.zc3o2UgvDzA3oHGH05uQBmUaULQb9OdlRCO4eSIfKKw",
              "_lv_for_bug_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVzd2TUxibkZfT18zZ1MtRTZTYVRkbDlK.Bews6_G-WYi2tQkYdUFMVRwgHfuTsjDOhUFKGlFfjWg",
              "_new_phx_thing_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYVDlGbWJiR0ZvdlhBUTJmdDd4R21KSENr.9DZqxBhUxC_4disJpQTKmGGdxjD_-besgywvgDUeStI",
              "_nnnnn_key" =>
                "SFMyNTY.g3QAAAABbQAAAAtfY3NyZl90b2tlbm0AAAAYaWFTdFF6V3dhTFVhbE96Z1NGVjNCTVBH.M5SpEDNbodElmj0iPdY3Wf4s56sZhMxTEYZBVB3LPfE",
              "_post_checkout_survey_key" =>
                "SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYNU5XTFJZSjRvTmlUNGZkN3I2SXFQMjg5bQAAAAlsYXN0X3NlZW50AAAACWQACl9fc3RydWN0X19kABRFbGl4aXIuTmFpdmVEYXRlVGltZWQACGNhbGVuZGFyZAATRWxpeGlyLkNhbGVuZGFyLklTT2QAA2RheWEHZAAEaG91cmEMZAALbWljcm9zZWNvbmRoAmIACcPgYQZkAAZtaW51dGVhBmQABW1vbnRoYQlkAAZzZWNvbmRhDWQABHllYXJiAAAH520AAAAHcHJldmlld2QABHRydWVtAAAACnNlc3Npb25faWRtAAAAFWpKZGFTTGtBclZvOWJCU1M2SHZCMW0AAAAHc2hvcF9pZG0AAAAVb3k5blJmVDRSck9UbzZLZ2RZN3p3.XeHQXPzOmkYdaQVjyqpkuy-p8nnRu2D6tdwbaB92tOs",
              "lb:probe_cookie" => "",
              "lb:session" =>
                "SFMyNTY.g3QAAAAFbQAAAAo4MDgwOnRva2VubQAAACDm9ONIw0zxpXZvvqpXq345ARj2LN3HhuvV9YmQdnN54G0AAAALX2NzcmZfdG9rZW5tAAAAGHVnRmt0bkxJZjFGX3dQQnVDZ0JOOGJXbG0AAAAPY3VycmVudF91c2VyX2lkbQAAACB4aW9hcnBodTM0eW42dnpmcHVrYnJpNnlnMjZ3YXpwdW0AAAANaWRlbnRpdHlfZGF0YXQAAAABZAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAAl1c2VyX2RhdGF0AAAABG0AAAAFZW1haWxkAANuaWxtAAAACWhleF9jb2xvcm0AAAAHI0ZBODA3Mm0AAAACaWRtAAAAIHhpb2FycGh1MzR5bjZ2emZwdWticmk2eWcyNndhenB1bQAAAARuYW1lZAADbmls.vHkagvvfi3SbQ7oGsnuh8hN_9zI_Pu4100A9cihQwD4",
              "twk_idm_key" => "cYY3tqqSg04zLnS6YawTb"
            }
          },
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{flash: %{}}, []},
          connect_params: %{},
          root_view: NnnnnWeb.MyTestLive,
          conn_session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: nil,
        root_pid: nil,
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      stacktrace: [
        {NnnnnWeb.MyTestLive, :handle_params, 3,
         [
           file: ~c"lib/nnnnn_web/live/my_test_live.ex",
           line: 28,
           error_info: %{module: Exception}
         ]},
        {Phoenix.LiveView.Utils, :"-call_handle_params!/5-fun-0-", 5,
         [file: ~c"lib/phoenix_live_view/utils.ex", line: 462]},
        {:telemetry, :span, 3, [file: ~c"/home/derek/Code/nnnnn/deps/telemetry/src/telemetry.erl", line: 321]},
        {Phoenix.LiveView.Static, :call_mount_and_handle_params!, 5,
         [file: ~c"lib/phoenix_live_view/static.ex", line: 279]},
        {Phoenix.LiveView.Static, :render, 3, [file: ~c"lib/phoenix_live_view/static.ex", line: 119]},
        {Phoenix.LiveView.Controller, :live_render, 3, [file: ~c"lib/phoenix_live_view/controller.ex", line: 39]},
        {Phoenix.Router, :__call__, 5, [file: ~c"lib/phoenix/router.ex", line: 432]},
        {NnnnnWeb.Endpoint, :plug_builder_call, 2, [file: ~c"lib/nnnnn_web/endpoint.ex", line: 1]},
        {NnnnnWeb.Endpoint, :"call (overridable 3)", 2, [file: ~c"deps/plug/lib/plug/debugger.ex", line: 136]},
        {NnnnnWeb.Endpoint, :call, 2, [file: ~c"lib/nnnnn_web/endpoint.ex", line: 1]},
        {Phoenix.Endpoint.SyncCodeReloadPlug, :do_call, 4,
         [file: ~c"lib/phoenix/endpoint/sync_code_reload_plug.ex", line: 22]},
        {Plug.Cowboy.Handler, :init, 2, [file: ~c"lib/plug/cowboy/handler.ex", line: 11]},
        {:cowboy_handler, :execute, 2, [file: ~c"/home/derek/Code/nnnnn/deps/cowboy/src/cowboy_handler.erl", line: 37]},
        {:cowboy_stream_h, :execute, 3,
         [file: ~c"/home/derek/Code/nnnnn/deps/cowboy/src/cowboy_stream_h.erl", line: 306]},
        {:cowboy_stream_h, :request_process, 3,
         [file: ~c"/home/derek/Code/nnnnn/deps/cowboy/src/cowboy_stream_h.erl", line: 295]},
        {:proc_lib, :init_p_do_apply, 3, [file: ~c"proc_lib.erl", line: 241]}
      ],
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"},
      kind: :error
    }
  end

  def handle_params_stop() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          connect_info: %{session: %{"_csrf_token" => "iaStQzWwaLUalOzgSFV3BMPG"}},
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          assign_new: {%{}, []},
          connect_params: %{
            "_csrf_token" => "WicVN2INLT9RI2QiXQY1LwAhFGNvIiAA3FFC3wzH0o1C1IOHSgBP-opG",
            "_mounts" => 5,
            "_track_static" => [
              "http://localhost:4000/assets/app.css",
              "http://localhost:4000/assets/app.js"
            ]
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {nil, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      uri: "http://localhost:4000/live?foo=bar",
      params: %{"foo" => "bar"}
    }
  end

  def handle_event_start() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints:
          {39_937_697_276_181_388_757_385_442_171_495_723_205,
           %{
             2 =>
               {73_617_967_197_300_037_744_481_173_618_097_897_040,
                %{
                  0 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  1 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  2 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {144_585_413_226_324_981_419_374_726_472_236_538_297,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }},
                  3 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {245_405_142_602_053_685_753_030_296_372_583_372_933,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }}
                }},
             3 =>
               {57_658_065_433_102_251_316_006_334_929_903_791_439,
                %{
                  0 =>
                    {111_334_535_098_600_973_939_295_926_462_112_722_006,
                     %{3 => {159_023_245_523_001_146_808_631_479_556_833_423_414, %{}}}}
                }}
           }},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      params: %{},
      event: "hello"
    }
  end

  def handle_event_start(:exception) do
    %{
      socket: %{
        id: "phx-F5N4Uz2FmAH4qwaD",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 540, 0),
        router: NnnnnWeb.Router,
        fingerprints:
          {39_937_697_276_181_388_757_385_442_171_495_723_205,
           %{
             2 =>
               {73_617_967_197_300_037_744_481_173_618_097_897_040,
                %{
                  0 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  1 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  2 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {144_585_413_226_324_981_419_374_726_472_236_538_297,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }},
                  3 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {245_405_142_602_053_685_753_030_296_372_583_372_933,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }}
                }},
             3 =>
               {33_984_160_809_218_664_543_582_638_264_118_753_933,
                %{
                  0 =>
                    {111_334_535_098_600_973_939_295_926_462_112_722_006,
                     %{3 => {159_023_245_523_001_146_808_631_479_556_833_423_414, %{}}}}
                }}
           }},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      params: %{},
      event: "hello"
    }
  end

  def handle_event_exception(:exception) do
    %{
      reason: %{message: "stop", __struct__: RuntimeError, __exception__: true},
      socket: %{
        id: "phx-F5N4Uz2FmAH4qwaD",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 540, 0),
        router: NnnnnWeb.Router,
        fingerprints:
          {39_937_697_276_181_388_757_385_442_171_495_723_205,
           %{
             2 =>
               {73_617_967_197_300_037_744_481_173_618_097_897_040,
                %{
                  0 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  1 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  2 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {144_585_413_226_324_981_419_374_726_472_236_538_297,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }},
                  3 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {245_405_142_602_053_685_753_030_296_372_583_372_933,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }}
                }},
             3 =>
               {33_984_160_809_218_664_543_582_638_264_118_753_933,
                %{
                  0 =>
                    {111_334_535_098_600_973_939_295_926_462_112_722_006,
                     %{3 => {159_023_245_523_001_146_808_631_479_556_833_423_414, %{}}}}
                }}
           }},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      stacktrace: [
        {NnnnnWeb.MyTestLive, :handle_event, 3,
         [
           file: ~c"lib/nnnnn_web/live/my_test_live.ex",
           line: 32,
           error_info: %{module: Exception}
         ]},
        {Phoenix.LiveView.Channel, :"-view_handle_event/3-fun-0-", 3,
         [file: ~c"lib/phoenix_live_view/channel.ex", line: 401]},
        {:telemetry, :span, 3, [file: ~c"/home/derek/Code/nnnnn/deps/telemetry/src/telemetry.erl", line: 321]},
        {Phoenix.LiveView.Channel, :handle_info, 2, [file: ~c"lib/phoenix_live_view/channel.ex", line: 221]},
        {:gen_server, :try_handle_info, 3, [file: ~c"gen_server.erl", line: 1077]},
        {:gen_server, :handle_msg, 6, [file: ~c"gen_server.erl", line: 1165]},
        {:proc_lib, :init_p_do_apply, 3, [file: ~c"proc_lib.erl", line: 241]}
      ],
      params: %{},
      kind: :error,
      event: "hello"
    }
  end

  def handle_event_stop() do
    %{
      socket: %{
        id: "phx-F5LbkYMazc6nbROF",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            handle_params: [],
            mount: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        assigns: %{__changed__: %{}, flash: %{}, live_action: :index},
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints:
          {39_937_697_276_181_388_757_385_442_171_495_723_205,
           %{
             2 =>
               {73_617_967_197_300_037_744_481_173_618_097_897_040,
                %{
                  0 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  1 => {33_937_828_406_068_249_838_328_265_119_631_535_217, %{}},
                  2 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {144_585_413_226_324_981_419_374_726_472_236_538_297,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }},
                  3 =>
                    {33_937_828_406_068_249_838_328_265_119_631_535_217,
                     %{
                       0 =>
                         {274_835_210_088_729_429_103_026_462_060_282_169_556,
                          %{
                            4 =>
                              {300_060_512_685_841_123_442_872_429_368_334_441_616,
                               %{
                                 1 =>
                                   {7_929_807_277_372_053_948_725_376_894_618_837_157,
                                    %{
                                      0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                                    }}
                               }},
                            5 =>
                              {245_405_142_602_053_685_753_030_296_372_583_372_933,
                               %{0 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}}},
                            7 => {206_108_695_073_089_763_669_587_230_053_011_233_172, %{}}
                          }}
                     }}
                }},
             3 =>
               {57_658_065_433_102_251_316_006_334_929_903_791_439,
                %{
                  0 =>
                    {111_334_535_098_600_973_939_295_926_462_112_722_006,
                     %{3 => {159_023_245_523_001_146_808_631_479_556_833_423_414, %{}}}}
                }}
           }},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      params: %{},
      event: "hello"
    }
  end

  def live_component_handle_event_start() do
    %{
      socket: %{
        id: "phx-F5LcqnujE9_EGAsB",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            mount: [],
            handle_params: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        assigns: %{
          id: 1,
          __changed__: %{},
          flash: %{},
          myself: %{__struct__: Phoenix.LiveComponent.CID, cid: 1}
        },
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {266_426_426_202_826_600_176_910_312_284_850_323_048, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      params: %{},
      event: "live component test",
      component: NnnnnWeb.MyTestLive.MyLiveComponent
    }
  end

  def live_component_handle_event_stop() do
    %{
      socket: %{
        id: "phx-F5LcqnujE9_EGAsB",
        private: %{
          __temp__: %{},
          lifecycle: %{
            handle_info: [],
            handle_event: [],
            __struct__: Phoenix.LiveView.Lifecycle,
            mount: [],
            handle_params: [],
            after_render: []
          },
          root_view: NnnnnWeb.MyTestLive
        },
        __struct__: Phoenix.LiveView.Socket,
        parent_pid: nil,
        endpoint: NnnnnWeb.Endpoint,
        view: NnnnnWeb.MyTestLive,
        assigns: %{
          id: 1,
          __changed__: %{},
          flash: %{},
          myself: %{__struct__: Phoenix.LiveComponent.CID, cid: 1}
        },
        transport_pid: IEx.Helpers.pid(0, 540, 0),
        root_pid: IEx.Helpers.pid(0, 548, 0),
        router: NnnnnWeb.Router,
        fingerprints: {266_426_426_202_826_600_176_910_312_284_850_323_048, %{}},
        redirected: nil,
        host_uri: %{
          port: 4000,
          scheme: "http",
          path: nil,
          host: "localhost",
          __struct__: URI,
          userinfo: nil,
          fragment: nil,
          query: nil,
          authority: nil
        }
      },
      params: %{},
      event: "live component test",
      component: NnnnnWeb.MyTestLive.MyLiveComponent
    }
  end
end
