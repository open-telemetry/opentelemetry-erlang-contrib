FROM elixir:1.20-alpine@sha256:ef915e5ab96e620116058c3b6002510227e4f5b57ca782296e675ee4e83d7344 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20-alpine@sha256:ef915e5ab96e620116058c3b6002510227e4f5b57ca782296e675ee4e83d7344 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
