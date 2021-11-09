## Contributing

### Instrumenting a library

When instrumenting a library, it is important to follow the [Trace Semantic Conventions](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/README.md).

### CI

Given the number of projects in this repo and the wide matrix of OTP/Elixir versions we must
test against, it is important to scope test jobs to the smallest subset of test possible. Project test jobs should utilize the test matrix strategy outputted by the `test-matrix` job.

### Test Matrix Updates

As new OTP and Elixir versions are released, the test matrix must be updated. A beautified version of the [test matrix](https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/.github/test-matrix.json) is kept for easy editing.

Updating the file:

1. Update the beautified json file (this is the form you'll commit)
2. Uglify and escape the json object
3. Copy this final value to the `test-matrix` job output
