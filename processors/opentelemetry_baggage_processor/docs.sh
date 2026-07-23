#!/bin/bash
set -e

# Setup:
#
#     # 1. install OTP 24+
#     # 2. install ExDoc:
#     $ mix escript.install github elixir-lang/ex_doc

rebar3 compile
rebar3 edoc

ex_doc "opentelemetry_baggage_processor" 0.0.1 "_build/default/lib/opentelemetry_baggage_processor/ebin" \
  --source-ref v0.0.1 \
  --config docs.config $@ \
  --output "doc"