defmodule RollDiceWeb.DiceController do
  use RollDiceWeb, :controller
  require OpenTelemetry.Tracer, as: Tracer

  def roll(conn, _params) do
    send_resp(conn, 200, roll_dice())
  end

  defp roll_dice do
    Tracer.with_span "roll_dice" do
      roll = Enum.random(1..6)

      Tracer.set_attribute("roll.value", roll)

      to_string(roll)
    end
  end
end
