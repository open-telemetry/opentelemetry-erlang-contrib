# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
import Config

try do
  import_config "#{Mix.env()}.exs"
rescue
  _ -> :ok
end
