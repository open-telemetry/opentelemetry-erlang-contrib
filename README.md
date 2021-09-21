---
<p align="center">
  <strong>
    <a href="https://github.com/open-telemetry/opentelemetry-erlang/blob/main/website_docs/getting-started.md">Getting Started<a/>
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
    <a href="examples/">Examples<a/>
  </strong>
</p>

---
[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
![Elixir](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/elixir.yml/badge.svg?branch=main)

## About this project

This is a repository for OpenTelemetry Erlang & Elixir contributions that are not part of the
[core repository](https://github.com/open-telemetry/opentelemetry-erlang) and
core distribution of the API and SDK.

## Instrumentations

OpenTelemetry can collect tracing data using instrumentation. Vendors/Users can also create and use their own. Currently, OpenTelemetry supports automatic tracing for:

### Elixir Instrumentation

- [opentelemetry-phoenix](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/instrumentation/opentelemetry_phoenix)

## Supported Runtimes

OpenTelemetry requires OTP v21.3 and above.

See [Elixir OTP Compatability](https://hexdocs.pm/elixir/1.12/compatibility-and-deprecations.html#compatibility-between-elixir-and-erlang-otp) for supported OTP/Elixir combinations.

## Contributing

We'd love your help!. Use tags [up-for-grabs][up-for-grabs-issues] and
[good first issue][good-first-issues] to get started with the project. Follow
[CONTRIBUTING](CONTRIBUTING.md) guide to report issues or submit a proposal.

## Useful links

- For more information on OpenTelemetry, visit: <https://opentelemetry.io/>
- For help or feedback on this project, join us in [GitHub Discussions][dhttps://github.com/open-telemetry/opentelemetry-erlang-contrib/discussions], `#otel-erlang-elixir` channel in the [CNCF slack](https://slack.cncf.io/), and `#opentelemetry` channel in the [Elixir Lang slack](https://elixir-slackin.herokuapp.com/)
- Erlang SIG [community page](https://github.com/open-telemetry/community#special-interest-groups)

## License

Apache 2.0 - See [LICENSE][license-url] for more information.

[discussions-url]: https://github.com/open-telemetry/opentelemetry-erlang-contrib/discussions
[license-url]: https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/LICENSE
[good-first-issues]: https://github.com/open-telemetry/openTelemetry-erlang-contrib/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
