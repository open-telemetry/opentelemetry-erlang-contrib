defmodule OpentelemetryRedix.CommandTest do
  use ExUnit.Case, async: true

  alias OpentelemetryRedix.Command

  test "sanitize commands that keep all args" do
    assert Command.sanitize(~w(GET mykey)) == "GET mykey"
  end

  test "sanitize commands that keep no args" do
    assert Command.sanitize(~w(AUTH password)) == "AUTH ?"
    assert Command.sanitize(~w(AUTH username password)) == "AUTH ? ?"
  end

  test "sanitize commands that keep first few args" do
    assert Command.sanitize(~w(SET mykey "value")) == "SET mykey ?"
  end

  test "sanitize commands with key-value args" do
    cmd = Command.sanitize(~w(MSET key1 "Hello" key2 "World"))
    assert cmd == "MSET key1 ? key2 ?"

    cmd = Command.sanitize(~w(HSET myhash field1 "Hello"))
    assert cmd == "HSET myhash field1 ?"
  end
end
