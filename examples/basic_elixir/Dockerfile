FROM elixir:1.20.4-alpine@sha256:3f25fa142baeaa0b0a62fbf8c10c96d8bfcf0abab0543ca4bec4ca8b19dd483a as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.4-alpine@sha256:3f25fa142baeaa0b0a62fbf8c10c96d8bfcf0abab0543ca4bec4ca8b19dd483a as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
