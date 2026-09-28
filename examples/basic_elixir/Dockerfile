FROM elixir:1.20.4-alpine@sha256:4805a3bc4efcb7568f45036811b2885418cb1d0b37664dcaf6e47983453323cb as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.4-alpine@sha256:4805a3bc4efcb7568f45036811b2885418cb1d0b37664dcaf6e47983453323cb as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
