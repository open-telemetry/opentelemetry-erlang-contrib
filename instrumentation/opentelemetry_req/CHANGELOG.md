# Changelog

## Unreleased

* Use span_name if provided, otherwise use path_params. If there are no
  path_params, default to request path

## 0.1.2

### Fixes

* Fix ctx not being set back to parent upon completion

## 0.1.1

### Fixes

* Fix client span to be the ctx injected to headers

## 0.1.0

### Features

* Initial release

