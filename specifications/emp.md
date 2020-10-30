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
message is represented by an envelope, containing a header, zero or more
extension blocks, and an optional body.

## Format
### Envelope
The envelope contains all parts of the message; it is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                             Size                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                            Content                            |
    |                              ...                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

The size of the envelope is a 32 bit unsigned integer, and represents the size
of the whole message, including the size field itself.

### Header
The header contains information about the message and its content.

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |      Type     |E|                 Unused                      |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Type: a 8 bit integer identifying the type of the message.
- E: a bit indicating if there is at least one extension block after the
  header.

### Extensions
Extensions are a mechanism to add new features to the protocol without
modifying its structure. Each message can include an arbitrary number of
extension block, each block containing information associated with an
extension.

An extension block is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                             Size                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    | Extension id  |M|                 Unused                      |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                            Content                            |
    |                              ...                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Size: the size of the entire extension block, including the size field, as a
  32 bit integer.
- Extension id: the numeric identifier of the extension as 8 bit integer.
- M: a bit indicating if there is more extension blocks after this one.
- Content: arbitrary data associated with the extension. The structure,
  encoding and meaning of any data in this field are defined by the extension
  identified by the extension id field.

Note that there can be multiple extension blocks associated with the same
extension.

### Body
The content of the body depends on the type of the message. Some message may
not have a body.

## Message types
EMP includes various kinds of messages. Each type of message is identified by
a 8 bit integer. Identifiers 0 to 127 are reserved; implementations are free
to introduce new message types for application specific use cases.

Implementations must support all message types described in this
document.

The following message types are currently defined:

| Identifier | Type    |
|------------|---------|
| 0          | `hello` |
| 1          | `bye`   |
| 2          | `ping`  |
| 3          | `pong`  |
| 4          | `error` |
| 5          | `data`  |

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
    |   Error code  | Extension id  |Ext. err. code |    Unused     |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         Error message                         |
    |                              ...                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Error code: a 8 bit integer identifying the error. Codes 0 to 127 are
  reserved.
- Extension id: if the error code is 3 (extension error), the 8 bit integer
  identifying the extension.
- Extension error code: if the error code is 3 (extension error), an 8 bit
  integer identifying the error. Refer to the documentation of the extension
  to interpret the code.
- Error message: a character string describing the error.

The following error codes are currently defined:

| Code | Description                                                                                            |
|------|--------------------------------------------------------------------------------------------------------|
| 0    | Unspecified error. Used for any error which does not match an existing code.                           |
| 1    | IO error. Used for any error associated with socket operations.                                        |
| 2    | Timeout. Indicate that a network operation timed out.                                                  |
| 3    | Protocol error. Indicates that an invalid message was received, or that the message flow is incorrect. |
| 4    | Extension error. Indicates that un invalid extension block was received.                               |

### Data
The `data` message is used to transfer application data. The content and
format of the body is defined by the application.

# Communication flow
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

## Data
Peers can send `data` messages at any moment. The format of the `data` message
is not defined by the EMP protocol; applications are free to represent data as
they see fit.

# Extensions
## Request-response
The request-response extension introduces a message exchange pattern where a
peer can send a request message and receive a correlated response.

The content of the extension block is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |R|                         Unused                              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                     Request identifier                        |
    |                                                               |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- R: a bit indicating whether the message is a request or a response. If set
  to 1, the message is a request. If set to 0, the message is a response.
- Request identifier: a 64 bit integer identifying the request-response pair.

An implementation must ensure it never send two requests with the same
identifier. The zero identifier is reserved and must not be used. For that
reason, implementations should keep a 64 bit counter starting at zero, and
increment it before sending each request.

After receiving a request and processing it, an implementation must send a
response with the same request identifier. Receiving a response with an
identifier which does not match any pending request is a protocol error.

## Compression
The compression extension allows compression of the message body.

The content of the extension block is encoded as follows:

     0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |     Scheme    |                    Unused                     |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Fields have the following meaning:

- Scheme: the compression scheme used for the body of the message.

The following compression scheme are supported:

| Scheme | Description                |
| ------ | -----------                |
| 0      | Identity (no compression). |
| 1      | GZIP (RFC 1952).           |
