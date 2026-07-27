# RollDice

The following example uses a basic [Phoenix](https://www.phoenixframework.org/)
web application. For more elaborate examples, see [examples](/docs/instrumentation/erlang/examples/).

To start your Phoenix server:

  * Run `mix setup` to install and setup dependencies
  * Start the opentelemetry collector and Jaeger front-end with `docker-compose up -d`
  * Set your service name `export OTEL_SERVICE_NAME="roll_dice"`
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

To generate and view traces:

  * Visit the new API endpoint with `curl http://localhost:4000/api/rolldice` or in your browser
  * Visit [`localhost:16686`](http://localhost:16686) to see the traces in Jaeger

For more information on the OpenTelemetry instrumentation in this example, check out the guide for getting started with [OpenTelemetry](https://opentelemetry.io/docs/instrumentation/erlang/getting-started/)

## How did we get here?

To begin, use the `phx` generator to create a new project. To keep things simple we'll leave out the database and html for now with the `--no-ecto` and `--no-html` flags.
In a real app, you'll probably want to include ecto configured for your preferred database with the `--database` flag (`postgres` by default).

```shell
mix phx.new --no-ecto --no-html roll_dice
```

### Rolling The Dice

Now we'll add an API endpoint that will let us roll the dice and return a random
number between 1 and 6.

To start, we'll add a route to the `/api` scope in your router

```elixir
# lib/roll_dice_web/router.ex
scope "/api", DiceGameWeb do
  pipe_through :api

  get "/rolldice", DiceController, :roll
end
```

Then we'll create a new file in the controllers folder for that module. We told
the router that we will define a roll function, so we'll do that. It will return
a `200` response code and the result of a `dice_roll` function, which we will
emit a span for. We also want to set the value of the generated roll as an
attribute on the span.

```elixir
# lib/roll_dice_web/controllers/dice_controller.ex
defmodule DiceGameWeb.DiceController do
  use DiceGameWeb, :controller
  require OpenTelemetry.Tracer, as: Tracer

  def roll(conn, _params) do
    send_resp(conn, 200, roll_dice())
  end

  defp roll_dice do
    Tracer.with_span("roll_dice") do
      roll = Enum.random(1..6)

      Tracer.set_attribute("roll.value", roll)

      to_string(roll)
    end
  end
end

```

If you point your browser/curl/etc. to [`localhost:4000/api/rolldice`](http://localhost:4000/api/rolldice) you should get a random number in response.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix
