# Changelog

## Unreleased

### Bug Fixes

* Handle `[:bandit, :request, :exception]` events without a `:conn`. The handler crashed with
  `{:badkey, :conn}` and was detached, so no HTTP server spans were produced until restart.

## [0.1.4] - 2023-12-14
### Changed
- Prepare to the public release
