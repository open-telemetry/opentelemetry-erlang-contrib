FROM elixir:1.20.5-alpine@sha256:4be7dbc192a2779867b8a78cc66debb0cde7eceb88c6084a085470ef87206b53 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.5-alpine@sha256:4be7dbc192a2779867b8a78cc66debb0cde7eceb88c6084a085470ef87206b53 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
