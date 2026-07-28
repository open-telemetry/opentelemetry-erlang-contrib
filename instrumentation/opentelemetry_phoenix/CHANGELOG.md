# Changelog

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
