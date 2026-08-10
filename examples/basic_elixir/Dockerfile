FROM elixir:1.20.2-alpine@sha256:bff1b504bc53b2e5f3b2716bd99c5654ca3a1c30e1a013e37f7ad3657482c6c1 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.2-alpine@sha256:bff1b504bc53b2e5f3b2716bd99c5654ca3a1c30e1a013e37f7ad3657482c6c1 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
