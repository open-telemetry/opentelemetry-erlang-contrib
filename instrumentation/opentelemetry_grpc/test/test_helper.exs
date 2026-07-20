Application.ensure_all_started(:opentelemetry)
Application.ensure_all_started(:opentelemetry_api)

ExUnit.start()
