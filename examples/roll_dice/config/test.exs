import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :roll_dice, RollDiceWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "ZclS6l1Z5RRwC1QKG8uIr4zgwQGkUYMB1AARGv3iaiC4GGRbATFBgKGKURgWPAMa",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
