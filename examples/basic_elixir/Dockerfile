FROM elixir:1.20-alpine@sha256:565e862f4d54ad1df5daa3776813d494d951353761d5ccb735c688cfc211f794 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20-alpine@sha256:565e862f4d54ad1df5daa3776813d494d951353761d5ccb735c688cfc211f794 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
