import Config

try do
  import_config "#{Mix.env()}.exs"
rescue
  _ -> :ok
end
