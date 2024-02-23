---
<p align="center">
  <strong>
    <a href="https://opentelemetry.io/docs/instrumentation/erlang/getting-started/">Getting Started<a/>
    &nbsp;&nbsp;&bull;&nbsp;&nbsp;
    <a href="https://hexdocs.pm/opentelemetry_api/1.0.0-rc.2/OpenTelemetry.html">API Documentation<a/>
  </strong>
</p>

<p align="center">
  <strong>
    <a href="CONTRIBUTING.md">Contributing<a/>
    &nbsp;&nbsp;&bull;&nbsp;&nbsp;
    <a href="instrumentation/">Instrumentation<a/>
    &nbsp;&nbsp;&bull;&nbsp;&nbsp;
    <a href="propagators/">Propagators<a/>
    &nbsp;&nbsp;&bull;&nbsp;&nbsp;
    <a href="utilities/">Utilities<a/>
    &nbsp;&nbsp;&bull;&nbsp;&nbsp;
    <a href="examples/">Examples<a/>
  </strong>
</p>

---
[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
![Elixir](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/elixir.yml/badge.svg?branch=main)

## About this project

This is a repository for OpenTelemetry Erlang & Elixir contribution libraries that are not part of the
[core repository](https://github.com/open-telemetry/opentelemetry-erlang) and
core distribution of the API and SDK.

## Instrumentations

OpenTelemetry can collect tracing data using instrumentation. Vendors/Users can also create and use their own. Currently, OpenTelemetry supports automatic tracing for:

- [opentelemetry-cowboy](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_cowboy)
- [opentelemetry-phoenix](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_phoenix)
- [opentelemetry-ecto](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_ecto)
- [opentelemetry-req](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_req)

## Supported Runtimes

OpenTelemetry Contribution libraries are verified to support the last 3 OTP versions
to match the supported runtimes of the OpenTelemetry API and SDK.

For Elixir libraries, all versions that support the last 3 OTP versions are verified.

See [Elixir OTP Compatibility](https://hexdocs.pm/elixir/1.16/compatibility-and-deprecations.html#compatibility-between-elixir-and-erlang-otp) for supported OTP/Elixir combinations.

Elixir libraries should aim for the minimum supported version to be the lowest possible
it can for the code in the library, but must at least support versions outlined above.

## Contributing

We'd love your help!. Use tags [up-for-grabs][up-for-grabs-issues] and
[good first issue][good-first-issues] to get started with the project. Follow
[CONTRIBUTING](CONTRIBUTING.md) guide to report issues or submit a proposal.

## Useful links

- For more information on OpenTelemetry, see [opentelemetry.io](https://opentelemetry.io).
- For conventions used in OpenTelemetry traces, see the following [readme](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/README.md).
- For help or feedback on this project, join us in [GitHub Discussions](https://github.com/open-telemetry/opentelemetry-erlang-contrib/discussions), `#otel-erlang-elixir` channel in the [CNCF slack](https://slack.cncf.io/), and `#opentelemetry` channel in the [Elixir Lang slack](https://elixir-slack.community/)
- Erlang SIG [community page](https://github.com/open-telemetry/community#special-interest-groups)

## License

Apache 2.0 - See [LICENSE](LICENSE) for more information.

[discussions-url]: https://github.com/open-telemetry/opentelemetry-erlang-contrib/discussions
[good-first-issues]: https://github.com/open-telemetry/openTelemetry-erlang-contrib/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
