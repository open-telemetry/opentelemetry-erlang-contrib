# Changelog

## 2.0.0-beta.1

## Features

- Semantic Conventions v1.27 support
- Support added for MySQL, MSSQL, and Sqlite
- Per-query options now available
- Connection settings now taken from init events for source of truth

### Breaking Changes

- Time measurement attributes are now namespaced to `ecto.*`
- Span names conform to SemConv 1.27 specification
- Several API changes. See the docs for more details

Special thank you to Dan Shultzer for his contributions.

## 1.2.0

### Breaking Changes

- `db.statement` attribute is now marked as optional. Add `db_statement: enabled` when calling `setup`

### Fixes

- Don't record DB statements without sanitizaiton

### Changed

- Add support for Elixir 1.15 and OTP 26
- Add required `db.system` attribute

## 1.1.1

### Changed

- Add db.name to ecto spans

## 1.1.0

### Changed

- Allow setting additional attributes

### Fixes

- Fix span linking in additional task-spawned use cases

## 1.0.0

### Changed

- Add idle time as an attribute

### Fixes

- Fix Ecto preload spans not being linked to the root parent query

## 1.0.0-rc.5

### Changed

- Opentelemetry 1.0 support

## 1.0.0-rc.4

### Changed

- Opentelemetry 1.0.0-rc.4 support

## 1.0.0-rc.2

### Changed

- Update dependencies to allow telemetry 1.0.0
