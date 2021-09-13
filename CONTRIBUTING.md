## Contributing


### CI

Given the number of projects in this repo and the wide matrix of OTP/Elixir versions we must
test against, it is important to scope test jobs to the smallest subset of test possible. Project test jobs should utilize the test matrix strategy outputted by the `test-matrix` job.

### Test Matrix Updates

As new OTP and Elixir versions are released, the test matrix must be updated. A beautified version of the [test matrix](https://github.com/open-telemetry/opentelemetry-erlang-contrib/blob/main/.github/test-matrix.json) is kept for easy editing which is automatically picked up in CI.
