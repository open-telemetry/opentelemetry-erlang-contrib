FROM elixir:1.20.4-alpine@sha256:5f967f23eade3de57cada6773f959677faba352bb5c063d46629726738dbdd28 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.4-alpine@sha256:5f967f23eade3de57cada6773f959677faba352bb5c063d46629726738dbdd28 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
