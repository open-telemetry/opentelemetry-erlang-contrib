# Changelog

## Unreleased

### Bug Fixes

* `setup/1` can now be called once per endpoint. The endpoint handler was
  attached under an id that did not vary with `:endpoint_prefix`, so
  applications running more than one endpoint silently lost the spans for
  every endpoint after the first. Reported in #537.

### Features

* LiveView `handle_event` spans now carry `http.route`. The route resolved during
  `mount`/`handle_params` is kept for the lifetime of the LiveView process, since
  `handle_event` telemetry metadata carries no URI and the socket keeps no path.
* Added the `liveview_span_names` option. It defaults to `:module`, which keeps the
  existing `MyAppWeb.ResourceLive.mount` names. Setting it to `:route` opts into
  `{operation} {route}` names, such as `live_view.mount /resources/:resource_id` and
  `live_view.handle_event /resources/:resource_id hello`, matching how span names are
  built elsewhere in the Semantic Conventions. The module name is used as the fallback
  when a LiveView is not mounted at the router.

## 2.0.0

### Features

- Added Bandit instrumentation support
- Semantic Conventions v1.27 support

### Breaking Changes

- Specifying an adapter is now required. Simply add the instrumentation
  library as a dep and follow its setup instructions, then specify your
  your adapter.
- Various HTTP Semantic Convention changes are included in the cowboy
  and bandit libraries. One major change regards span naming. This may
  affect your observability tools when keying on span names. The key
  change there is the HTTP method is now a prefix, e.g. "GET /users/:user_id"
- OpenTelemetry API v1.4 required

## 2.0.0-rc.1

### Features

- Added Bandit instrumentation support
- Semantic Conventions v1.27 support

### Breaking Changes

- Specifying an adapter is now required. Simply add the instrumentation
  library as a dep and follow its setup instructions, then specify your
  your adapter.
- Various HTTP Semantic Convention changes are included in the cowboy
  and bandit libraries. One major change regards span naming. This may
  affect your observability tools when keying on span names. The key
  change there is the HTTP method is now a prefix, e.g. "GET /users/:user_id"
- OpenTelemetry API v1.4 required

## 1.2.0

### Features

- Add support for LiveView courtesy of @derekkraan

### Fixes

- Do not set a span as errored for exceptions, only based on 5xx HTTP status

### Changed

- Minimum supported Elixir version changed to 1.11.

## 1.1.1

### Fixes

- [Relax nimble_options
  requirement](https://github.com/open-telemetry/opentelemetry-erlang-contrib/pull/161)

## 1.1.0

### Features

- Add support for opentelemetry_cowboy to capture the full request lifecycle
  when using the Plug.Cowboy adapter

## 1.0.0

### Fixes

- Prevent attempting to record an exception when no active span present
- Only mark 5xx level status codes as errored

## 1.0.0-rc.7

### Changed

- Opentelemetry 1.0 support

## 1.0.0-rc.6

### Changed

- Opentelemetry 1.0.0-rc.4 support

### Fixes

- pass attributes on span start for better sampling options
- fix http status attribute to match spec

## 1.0.0-rc.4

### Changed

- Opentelemetry dependency is locked to rc2 or lower in prep for breaking changes in rc3

## 1.0.0-rc.3

### Changed

- Update dependencies to allow telemetry 1.0.0

## 0.2.0

### Changed

- Upgraded to Opentelemetry v0.5.0
