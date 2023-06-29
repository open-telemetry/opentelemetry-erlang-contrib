ExUnit.start()

{:ok, _} = Application.ensure_all_started(:hackney)
{:ok, _} = Application.ensure_all_started(:opentelemetry_api)
{:ok, _} = Application.ensure_all_started(:opentelemetry)
