# Changelog

## Unreleased

### Fixed

* Fix `OpentelemetryOban.PluginHandler` crashing with `:badarg` when an
  `Oban.Plugins.Cron` (or `Oban.Pro.Plugins.DynamicCron`) `[:oban, :plugin, :stop]`
  event carries no `:jobs` in its metadata. Oban omits `:jobs` when the scheduled
  insert fails (e.g. a transient DB error), so `length(metadata[:jobs])` raised on
  `nil` and `:telemetry` detached the handler, silently disabling plugin tracing
  until the next restart. `jobs_count` now defaults to `0`.

* Report the real discarded and rescued counts for `Oban.Plugins.Lifeline` and
  `Oban.Pro.Plugins.DynamicLifeline`. Both read `:discarded_count` and
  `:rescued_count`, which Oban does not emit; the plugins report the
  `:discarded_jobs` and `:rescued_jobs` lists instead. The attributes were
  therefore always `nil`, which the exporter shipped as the string `"nil"` rather
  than as a count.

* Record `exception.message` on the `exception` event of plugin spans.
  `OpentelemetryOban.PluginHandler` recorded exceptions through
  `:otel_span.record_exception/5`, whose Erlang API cannot read an Elixir exception
  struct: it recorded no `exception.message` and an `exception.type` holding an
  inspected Erlang map rather than the exception module. Reasons are now normalised
  with `Exception.normalize/3` and recorded through
  `OpenTelemetry.Span.record_exception/4`. Reasons that cannot be normalised
  (`:throw` and `:exit` kinds) still fall back to the Erlang API.

* Mark plugin spans as errored when a `[:oban, :plugin, :stop]` event carries an
  `:error`. Plugins report failure through the `{:error, meta}` return value of
  `:telemetry.span/3`, which emits a `:stop` event rather than an `:exception`
  event, so a failed plugin run produced a span with an `:unset` status that was
  indistinguishable from success. Every plugin except `Oban.Stager` reports its
  whole `{:error, reason}` return value, so the reason is unwrapped before the
  status description and `error.type` are derived from it. A `nil` reason, which
  `Oban.Plugins.Reindexer` reports on every non-leader node because it has no
  `else` branch, is a no-op rather than a failure and is left unset.

* Read job failure metadata from the documented `:reason` key in
  `OpentelemetryOban.JobHandler`. It matched on `:error`, an undocumented alias
  that Oban emits alongside `:reason`, so the handler would stop matching and
  crash if Oban ever dropped it.

### Changed

* Emit plugin module names in their short form, without the BEAM-internal
  `Elixir.` prefix. Both the `oban.plugin` attribute and the plugin span name
  carried the module in a form the exporter renders as `"Elixir.Oban.Plugins.Cron"`.
  The [Configuration Options](../../docs/reference/configuration-options.md)
  reference requires the short form, and Oban itself already stores `worker` that
  way, so the attribute is now `"Oban.Plugins.Cron"` and the span name is now
  `"Oban.Plugins.Cron process"`. Consumers filtering on the prefixed attribute or
  grouping on the prefixed span name must be updated.

## 1.1.1

### Fixed

* Fixes broken handling and recording of Oban Plugin exceptions

## 1.1.0

### Changed

* Improve `OpentelemetryOban.PluginHandler` Tracer span attributes.
  The Plugin's span introduce a set of attributes prefixed with `oban.`.
  Previously, no attributes were added to the span. The new attributes are:

  * All Plugin:
    * `oban.plugin`
  * `Oban.Plugins.Cron` Plugin:
    * `oban.jobs_count`
  * `Oban.Plugins.Gossip` Plugin:
    * `oban.gossip_count`
  * `Oban.Plugins.Lifeline` Plugin:
    * `oban.discarded_count`
    * `oban.rescued_count`
  * `Oban.Plugins.Pruner` Plugin:
    * `oban.pruned_count`
  * `Oban.Pro.Plugins.DynamicCron` Plugin:
    * `oban.jobs_count`
  * `Oban.Pro.Plugins.DynamicLifeline` Plugin:
    * `oban.discarded_count`
    * `oban.rescued_count`
  * `Oban.Pro.Plugins.DynamicPrioritizer` Plugin:
    * `oban.reprioritized_count`
  * `Oban.Pro.Plugins.DynamicPruner` Plugin:
    * `oban.pruned_count`
  * `Oban.Pro.Plugins.DynamicScaler` Plugin:
    * `oban.scaler.last_scaled_to`
    * `oban.scaler.last_scaled_at`

## 1.0.0

### Changed

* Publish 1.0

### Fixes

* Fix issue with insert_all

## 0.2.0-rc.5

### Changed

* Opentelemetry 1.0 support

## 0.2.0-rc.4

### Changed

* Opentelemetry 1.0.0-rc.4 support

## 0.1.0

* Initial release
