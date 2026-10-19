FROM elixir:1.20.4-alpine@sha256:f1a8c06b332cacc7239381a25331a807fb63962f8221dff545ea4d8f78b0f50c as builder
RUN mix local.hex --force && mix local.rebar --force
WORKDIR /app
COPY . /app
RUN mix deps.get
RUN MIX_ENV=prod mix release

FROM elixir:1.20.4-alpine@sha256:f1a8c06b332cacc7239381a25331a807fb63962f8221dff545ea4d8f78b0f50c as app
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/basic_elixir .
CMD bin/basic_elixir start
