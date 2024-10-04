## Contributing

### Library Inclusion in the Erlang Contrib Repository Policy

Maintaining libraries can be a demanding task. Due to the limited number of maintainers, any library added to the
contrib repository must have an OpenTelemetry CNCF Erlang Contrib Approver or Maintainer as a sponsor. [Becoming a member](https://github.com/open-telemetry/community/blob/main/guides/contributor/membership.md) and approver is a simple
process. Following approval, the library can be merged and you will be added as a codeowner for it.

This policy has been enacted to be respectful of the maintainers' time and to ensure users get timely responses
and regular updates. We want as many libraries to be under the official umbrella but need your commmitment to
make that happen.

### Instrumenting a library

When instrumenting a library, it is important to follow the [Trace Semantic Conventions](https://github.com/open-telemetry/semantic-conventions/blob/main/docs/general/trace.md)

### CI

Given the number of projects in this repo and the wide matrix of OTP/Elixir versions we must
test against, it is important to scope test jobs to the smallest subset of test possible. Project test jobs should utilize the test matrix strategy outputted by the `test-matrix` job.

### Test Matrix Updates

As new OTP and Elixir versions are released, the test matrix must be updated. A beautified version of the [test matrix](https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/.github/test-matrix.json) is kept for easy editing.

Updating the file:

1. Update the beautified json file (this is the form you'll commit)
2. Uglify and escape the json object
3. Copy this final value to the `test-matrix` job output
