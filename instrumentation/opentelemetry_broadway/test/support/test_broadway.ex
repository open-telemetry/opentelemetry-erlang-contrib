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
        default: [
          batch_size: 2,
          batch_timeout: 10
        ]
      ]
    )
  end

  @impl true
  def handle_message(_processor, %Broadway.Message{} = message, _context) do
    case message.data do
      "success" -> message
      "error" -> Broadway.Message.failed(message, "something went wrong")
      "exception" -> raise RuntimeError, "an exception occurred"
    end
  end

  @impl true
  def handle_batch(_batcher, messages, _batch_info, _context) do
    messages
  end
end
