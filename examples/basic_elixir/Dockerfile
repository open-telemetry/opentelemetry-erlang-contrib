FROM elixir:1.20.3-alpine@sha256:8e4dcf7e98d3e06f4a6f56ed40e65b33b6bd84b0779698c8562d61d06218f314 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.3-alpine@sha256:8e4dcf7e98d3e06f4a6f56ed40e65b33b6bd84b0779698c8562d61d06218f314 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
