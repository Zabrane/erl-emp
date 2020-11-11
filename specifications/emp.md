% EMP

# Introduction
EMP, for Extensible Messaging Protocol, is a communication protocol designed
for high performance bidirectional messaging.

# Encoding
EMP messages use a simple binary encoding.

## Integers
Integers are always represented in network byte order, also known as
big-endian. Integers are unsigned unless stated otherwise. Signed integers use
a two's complement representation.

## Character strings
Character strings are represented as UTF-8 byte sequences, preceded by the
length of the sequence as a 16 bit integer. For example, the character string
`hello` is represented by the byte sequence `00 05 68 65 6c 6c 6f`.

# Transport
EMP is designed to run over a reliable, connection-oriented, stream-based
transport protocol. The reference implementation supports TCP and TLS.

# Messages
A message is a unit of information transmited from one peer to the other. Each
message is represented by an envelope containing a header and an optional
body.

## Format
### Header
The header contains information about the message and its content.

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |      Type     |                   Unused                      |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


Fields have the following meaning:

- Type: a 8 bit integer identifying the type of the message.

### Body
The content of the body depends on the type of the message. Some message may
not have a body.

## Message types
EMP includes various kinds of messages. Each type of message is identified by
a 8 bit integer.

The following message types are currently defined:

| Code | Type       |
|------|------------|
| 0    | `hello`    |
| 1    | `bye`      |
| 2    | `ping`     |
| 3    | `pong`     |
| 4    | `error`    |
| 5    | `request`  |
| 6    | `response` |

### Hello
The `hello` message is sent by a peer after the connection has been
established.

The body is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |    Version    |                   Unused                      |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Version: the version number of the protocol supported by the sender as a 8
  bit integer. The current version is 1.

### Bye
The `bye` message is used to indicate that the sender is going to close the
connection for a reason other than an error.

This message does not have a body.

### Ping
Peers can send `ping` messages regularly as an alternative to the TCP
keep-alive mechanism.

This message does not have a body.

### Pong
Peers send a `pong` message when they receive a `ping` message.

This message does not have a body.

### Error
The `error` message indicates that the sender encountered an error. The sender
must close the connection after sending an `error` message. A peer receiving
an `error` message must close the connection.

The body is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |   Error code  |                    Unused                     |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                          Description                          |
    |                              ...                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Code: a 8 bit integer identifying the error.
- Description: a character string describing the error.

The following error codes are currently defined:

| Code | Description                                                                                            |
|------|--------------------------------------------------------------------------------------------------------|
| 0    | Internal error. Used for system and network errors.                                                    |
| 1    | Protocol error. Indicates that an invalid message was received, or that the message flow is incorrect. |
| 2    | Service unavailable.                                                                                   |
| 3    | Invalid request identifier.                                                                            |
| 4    | Invalid request.                                                                                       |

### Request
The `request` message is used to send messages which expect a response.

The body is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         Identifier                            |
    |                                                               |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                           Data                                |
    |                            ...                                |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Identifier: a 64 bit integer identifying the request.
- Data: application data; the content and format of this field is defined in
the [Application data](#application-data) section.

### Response
The `response` message is used to reply to requests.

The body is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         Identifier                            |
    |                                                               |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                           Data                                |
    |                            ...                                |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Identifier: a 64 bit integer identifying the request the response is
  associated with.
- Data: application data; the content and format of this field is defined in
the [Application data](#application-data) section.

# Communication flow
## Connection
A client can connect to a server at any time. If a connection dies, the client
can try to reconnect; implementations should implement a backoff mechanism to
avoid overloading the network and the server.

After a connection has been established, peers start the handshake procedure.

## Handshake
When the connection has been established, each peer starts the handshake
procedure:

- Send a `hello` message. The version field is set to the highest version
  number of the protocol supported by the sender.
- Wait for a message:
  - if this is a `bye` or `error` message, close the connection and terminate;
  - if this is a `hello` message, check that the version field contains a
    supported version number: if it does, the handshake is successful; if it
    does not, close the connection and terminate.
  - If this is any other message, close the connection and terminate.

After conclusion of the handshake, peers can engage in any of the exchanges
described below. These exchanges must not be initiated during the handshake
procedure.

## Termination
Peers can terminate the exchange at any moment one of the following ways:

- Sending a `bye` message and closing the connection, for a normal termination.
- Sending an `error` message and closing the connection, for a termination due
  to an error that cannot be recovered.
- Closing the connection.

Closing the connection without sending a `bye` or `error` message should be
considered a last resort.

## Ping-pong
Peers can initiate a ping-pong procedure at any time by sending `ping`
message. The procedure terminates at the reception of the first subsequent
`pong` message received. Any other message received in the mean time is
processed as usual.

A peer having started a ping-pong procedure must not send any additional
`ping` message until the termination of the current procedure.

Peers should configure a timeout value indicating the maximum amount of time
to wait for the reception of the `pong` message. If the `pong` message is not
received in this delay, the initiator of the ping-pong procedure should
consider the connection inactive and close it. In that situation, it may try
to send an `error` message before closing the connection.

## Requests and responses
Peers can send `request` messages at any moment. Peers can implement
pipelining, i.e. sending new requests without waiting for the response to any
previously sent request. Implementations are free to restrict pipelining
partially or entirely.

Requests must be processed in order: peers must not process a request unless
the response to the previous request has been sent.

Each request is identified by a unique 64 bit integer. The zero identifier
must never be used. An implementation must ensure it never send two requests
with the same identifier. Implementations should keep a 64 bit counter
starting at zero and increment it after sending each request.

After receiving a request and processing it, an implementation must send a
`response` message with the same request identifier. Receiving a response with
an identifier which does not match any pending request must trigger an error
with code 2 (invalid request identifier).

# Application data
This section describes the format used for application data in the current
version of the specification.

Application data are represented as JSON values. JSON serialization must use
UTF-8. The resulting byte sequence must not start with byte order mark.

Requests are represented as JSON objects with the following fields:

- `op`: the operation to execute as a string.
- `data`: an object containing arbitrary data associated with the operation
  (optional).

Example:
```json
{
  "op": "cancel_job",
  "data": {
    "job_id": "ee20d7b2-0887-48bf-9982-3b9b3730f26d",
    "reason": "workflow_deleted"
  }
}
```

Responses are represented as JSON objects with the following fields:

- `status`: the status of the operation, either `"success"` or `"failure"`.
- `code`: an error code identifying the cause of the failure as a string.
- `description`: a description of what caused the failure (mandatory if the
  status is `"failure"`).
- `data`: an object containing arbitrary data associated with either the
  success or the failure of the operation (optional).

Example:
```json
{
  "status": "failure",
  "code": "unknown_job",
  "description": "unknown job ee20d7b2-0887-48bf-9982-3b9b3730f26d",
  "data": {
    "job_id": "ee20d7b2-0887-48bf-9982-3b9b3730f26d"
  }
}
```
