# OpentelemetryBandit

Telemetry handler that creates Opentelemetry spans from [Bandit events](https://hexdocs.pm/bandit/Bandit.Telemetry.html#content).

After installing, setup the handler in your application behaviour before your top-level supervisor starts.

```elixir
OpentelemetryBandit.setup()
```

When phoenix is used, setup telemetry this way:

```elixir
OpentelemetryBandit.setup()
OpentelemetryPhoenix.setup(adapter: :bandit)
```

*Note:* Please, vote for the PR to opentelemetry-phoenix: https://github.com/open-telemetry/opentelemetry-erlang-contrib/pull/249

## Installation

```elixir
def deps do
  [
    {:opentelemetry_bandit, "~> 0.1.4"}
  ]
end
```
