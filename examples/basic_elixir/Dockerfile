FROM elixir:1.20.5-alpine@sha256:a2f0fe1bfd8a2f9cc91d913f4a12e6430e27e618720f753a514e2fff3d35f5a7 as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.5-alpine@sha256:a2f0fe1bfd8a2f9cc91d913f4a12e6430e27e618720f753a514e2fff3d35f5a7 as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
