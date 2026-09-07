# opentelemetry_diameter

Metrics collector to export statistics from the diameter application.

The metrics and their names are heavily borrowed from [prometheus.erl](https://github.com/deadtrickster/prometheus.erl).

After installing, setup the desired metrics in your application behaviour before your
top-level supervisor starts. Make sure the API and SDK applications are started before
your application.

```erlang
opentelemetry_diameter_metrics:setup(),
...
```

## Metrics

### `diameter.application.count`

Number of installed DIAMETER applications. Unit: `{application}`

| Attribute                 | Description      |
| ------------------------- | ---------------- |
| `diameter.service.name` | The service name |

### `diameter.connection.count`

Number of connections to peers. Unit: `{connection}`

| Attribute | Description |
|---|---|
| `diameter.service.name` | The service name |
| `diameter.peer.origin_host` | The `Origin-Host` of the peer [1] |
| `diameter.peer.origin_realm` | The `Origin-Realm` of the peer [2] |
| `network.transport` | The transport used for the connection, `tcp` or `sctp` |
| `diameter.connection.watchdog.state` | The state of the connection watchdog |
| `diameter.role` | The role of the node in the connection, `initiator` or `responder` |
| `network.local.address` | The local IP address of the connection |
| `network.local.port` | The local port of the connection |
| `network.peer.address` | The peer IP address of the connection |
| `network.peer.port` | The peer port of the connection |

[1] **Note:** this is the `Origin-Host` of the connection peer and not the `Origin-Host` from the diameter message exchanged.
[2] **Note:** this is the `Origin-Realm` of the connection peer and not the `Origin-Realm` from the diameter message exchanged.

### `diameter.message.count`

Number of requests. Unit: `{request}`

| Attribute | Description |
|---|---|
| `diameter.service.name` | The service name |
| `diameter.peer.origin_host` | The `Origin-Host` of the peer [1] |
| `message.direction` | The direction of the message, `sent` or `received` |
| `diameter.command.type` | The type of the command, `request` or `answer` |
| `diameter.application.id` | The application ID of the command |
| `diameter.command.code` | The command code |
| `diameter.command.name` | The name of the command |
| `diameter.result_code` | The result code of the answer, only for answers |

[1] **Note:** this is the `Origin-Host` of the connection peer and not the `Origin-Host` from the diameter message exchanged.

### `diameter.connection.io`

Bytes sent/received over a connection. Unit: `By`

| Attribute | Description |
|---|---|
| `diameter.service.name` | The service name |
| `diameter.peer.origin_host` | The `Origin-Host` of the peer [1] |
| `diameter.peer.origin_realm` | The `Origin-Realm` of the peer [2] |
| `network.transport` | The transport used for the connection, `tcp` or `sctp` |
| `diameter.connection.watchdog.state` | The state of the connection watchdog |
| `diameter.role` | The role of the node in the connection, `initiator` or `responder` |
| `network.local.address` | The local IP address of the connection |
| `network.local.port` | The local port of the connection |
| `network.peer.address` | The peer IP address of the connection |
| `network.peer.port` | The peer port of the connection |
| `network.io.direction` | The direction of the IO, `receive` or `transmit` |

[1] **Note:** this is the `Origin-Host` of the connection peer and not the `Origin-Host` from the diameter message exchanged.
[2] **Note:** this is the `Origin-Realm` of the connection peer and not the `Origin-Realm` from the diameter message exchanged.

### `diameter.connection.packets`

Packets sent/received over a connection. Unit: `{packet}`

| Attribute | Description |
|---|---|
| `diameter.service.name` | The service name |
| `diameter.peer.origin_host` | The `Origin-Host` of the peer [1] |
| `diameter.peer.origin_realm` | The `Origin-Realm` of the peer [2] |
| `network.transport` | The transport used for the connection, `tcp` or `sctp` |
| `diameter.connection.watchdog.state` | The state of the connection watchdog |
| `diameter.role` | The role of the node in the connection, `initiator` or `responder` |
| `network.local.address` | The local IP address of the connection |
| `network.local.port` | The local port of the connection |
| `network.peer.address` | The peer IP address of the connection |
| `network.peer.port` | The peer port of the connection |
| `network.io.direction` | The direction of the IO, `receive` or `transmit` |

[1] **Note:** this is the `Origin-Host` of the connection peer and not the `Origin-Host` from the diameter message exchanged.
[2] **Note:** this is the `Origin-Realm` of the connection peer and not the `Origin-Realm` from the diameter message exchanged.

### `diameter.error.count`

Number of errors. Unit: `{error}`

| Attribute | Description |
|---|---|
| `diameter.service.name` | The service name |
| `diameter.peer.origin_host` | The `Origin-Host` of the peer [1] |
| `message.direction` | The direction of the message, `sent` or `received` |
| `diameter.command.type` | The type of the command, `request` or `answer` |
| `diameter.application.id` | The application ID of the command |
| `diameter.command.code` | The command code |
| `diameter.command.name` | The name of the command |
| `diameter.error.type` | The type of the error |

[1] **Note:** this is the `Origin-Host` of the connection peer and not the `Origin-Host` from the diameter message exchanged.
