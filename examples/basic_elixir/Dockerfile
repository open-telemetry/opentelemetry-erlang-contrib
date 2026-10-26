FROM elixir:1.20.4-alpine@sha256:f95251968625b0b61716355b372b58b1916bb618b949de3d2e62fb803ad68784 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.4-alpine@sha256:f95251968625b0b61716355b372b58b1916bb618b949de3d2e62fb803ad68784 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
