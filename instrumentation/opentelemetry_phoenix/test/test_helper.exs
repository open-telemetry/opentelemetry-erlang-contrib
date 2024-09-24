ExUnit.start()
Code.put_compiler_option(:warnings_as_errors, true)

otp_vsn =
  :erlang.system_info(:otp_release)
  |> to_string()
  |> String.to_integer()

if otp_vsn >= 27 do
  ExUnit.configure(exclude: [integration: true])
else
  ExUnit.configure(exclude: [integration: false])
end
