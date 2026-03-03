# Changelog

## 2.5.0

### Features

- Add NimbleOptions schema validation for all configuration options
- Add `extra_attrs` option for user-defined span attributes
- Add `request_header_attrs` and `response_header_attrs` options
- Add opt-in semantic convention attributes via `opt_in_attrs`

### Breaking Changes

- HTTP method attribute values are now atoms (`:GET`) instead of strings (`"GET"`)
- Body size attributes (`http.request.body.size`, `http.response.body.size`) are now integers instead of strings
- URL scheme attributes are now atoms (`:http`, `:https`) instead of strings
