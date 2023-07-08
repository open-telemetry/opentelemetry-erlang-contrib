# Changelog

## 1.3.0

### Changed

- `OpentelemetryHTTPoison.setup/1` is now deprecated
- OpentelemetryHTTPoison configuration is now read from `Application` env

### Fixed

- configuration options are correctly validated during `setup` instead of upon retrieval
- request options (e.g. `:ot_resource_route` and `:ot_attributes`) can be correctly passed together to OpentelemetryHTTPoison calls (e.g. `get`)
- non valid `:ot_attributes` are ignored instead of being mapped to `nil`
- usages of `:infer_fn` are now all `:infer_route`, which is the correct option

## 1.2.2

### Fixed

- Trace propagation breaking if one of the header keys was an atom instead of string.
  Note that this also causes HTTPoison.Response{request: %{headers}} to always use string for header keys.

## 1.2.1

### Fixed

- `OpentelemetryHTTPoison.request` will work even if `OpentelemetryHTTPoison.setup` hasn't been called

## 1.2.0

### Added

- New `:ot_attributes` option to set default Open Telemetry metadata attributes to be added to each OpentelemetryHTTPoison request
- New otel semantic conventions library to ensure proper conventions are followed

### Changed

- Span name contains `method` only now, as per semantic conventions
- `http.url` will be stripped of credentials. (eg. if the url is `"https://username:password@www.example.com/"` the attribute's value will be `"https://www.example.com/"`)

## 1.1.2

### Added

- New `"net.peer.name"` attribute
- HTTPoison 2.0.0 is now supported
