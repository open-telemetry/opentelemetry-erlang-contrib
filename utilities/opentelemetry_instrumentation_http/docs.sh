#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang 24.0.2
#     asdf local erlang 24.0.2

rebar3 compile
rebar3 as docs edoc
version=0.1.0
ex_doc "opentelemetry_instrumentation_http" $version "_build/default/lib/opentelemetry_instrumentation_http/ebin" \
       --source-ref v${version} \
       --config docs.config $@
