import Config

try do
  import_config "#{config_env()}.exs"
rescue
  _ -> :ok
end
