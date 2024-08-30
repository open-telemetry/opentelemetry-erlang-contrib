defmodule TestBroadway do
  use Broadway

  def start_link() do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {Broadway.DummyProducer, []}
      ],
      processors: [
        default: []
      ],
      batchers: [
        # Set a predictable batch_size so our batch tests will work as expected
        default: [batch_size: 2]
      ]
    )
  end

  @impl true
  def handle_message(_processor, %Broadway.Message{} = message, _context) do
    case message.data do
      "error" -> Broadway.Message.failed(message, "something went wrong")
      "exception" -> raise RuntimeError, "an exception occurred"
      _ -> message
    end
  end

  @impl true
  def handle_batch(_batcher, messages, _batch_info, _context) do
    # Broadway.test_batch/3 will pass `messages` through `handle_message/4` before it hits
    # handle_batch/3. Thus we need to differentiate a message failed during "batching" vs "intake"
    Enum.map(messages, fn message ->
      case message.data do
        "batch error" -> Broadway.Message.failed(message, "something went wrong")
        "batch exception" -> raise RuntimeError, "an exception occurred"
        _ -> message
      end
    end)
  end
end
