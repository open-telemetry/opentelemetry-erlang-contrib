defmodule RollDiceWeb.DiceController do
  use RollDiceWeb, :controller
  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetryAPIExperimental.Counter, as: Counter

  def roll(conn, _params) do
    send_resp(conn, 200, roll_dice())
  end

  defp roll_dice do
    Counter.add(:number_of_requests, 1)

    Tracer.with_span "roll_dice" do
      roll = Enum.random(1..6)

      Tracer.set_attribute("roll.value", roll)

      to_string(roll)
    end
  end
end
