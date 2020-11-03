%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(emp_proto).

-export([hello_message/0, bye_message/0, ping_message/0, pong_message/0,
         error_message/2, error_message/3, data_message/1,
         encode_envelope/1, encode_message/1,
         decode_message/1,
         encode_string/1, decode_string/1]).

-export_type([message_type/0, message/0,
              extension/0, extension_id/0,
              message_body/0, hello_message_body/0, error_message_body/0,
              data_message_body/0,
              version/0, error_code/0,
              request_response_extension/0, request_id/0,
              compression_extension/0, compression_scheme/0]).

-type message_type() :: hello | bye | ping | pong | error | data.

-type message() :: #{type := message_type(),
                     extensions => [extension()],
                     body => message_body()}.

-type message_body() :: hello_message_body()
                      | error_message_body()
                      | data_message_body().

-type hello_message_body() :: #{version := version()}.

-type error_message_body() :: #{code := error_code(),
                                description := binary()}.

-type data_message_body() :: iodata().

-type version() :: 0..255.

-type error_code() :: unspecified_error
                    | io_error
                    | timeout
                    | protocol_error
                    | extension_error.

-type extension() :: {request_response, request_response_extension()}
                   | {compression, compression_extension()}.

-type extension_id() :: 0..255.

-type request_response_extension() :: #{type := request | response,
                                        id := request_id()}.

-type request_id() :: 1..18446744073709551615.

-type compression_extension() :: #{scheme := compression_scheme()}.

-type compression_scheme() :: identity | gzip.

-spec hello_message() -> message().
hello_message() ->
  #{type => hello, body => #{version => 1}}.

-spec bye_message() -> message().
bye_message() ->
  #{type => bye}.

-spec ping_message() -> message().
ping_message() ->
  #{type => ping}.

-spec pong_message() -> message().
pong_message() ->
  #{type => pong}.

-spec error_message(error_code(), binary() | string()) -> message().
error_message(Code, Message) ->
  #{type => error,
    body => #{code => Code,
              description => unicode:characters_to_binary(Message)}}.

-spec error_message(error_code(), io:format(), [term()]) -> message().
error_message(Code, Format, Args) ->
  Message = io_lib:format(Format, Args),
  error_message(Code, iolist_to_binary(Message)).

-spec data_message(Body :: iodata()) -> message().
data_message(Body) ->
  #{type => data, body => Body}.

-spec encode_envelope(message()) -> iodata().
encode_envelope(Message) ->
  Data = encode_message(Message),
  Size = iolist_size(Data),
  [<<Size:32>>, Data].

-spec encode_message(message()) -> iodata().
encode_message(Message = #{type := Type}) ->
  Extensions = maps:get(extensions, Message, []),
  BodyData = case maps:find(body, Message) of
               {ok, Body} ->
                 encode_body(Body);
               error ->
                 []
             end,
  E = case Extensions of
        [] -> 0;
        _ -> 1
      end,
  Header = <<(encode_message_type(Type)):8, E:1, 0:23>>,
  [Header, encode_extensions(Extensions), BodyData].

-spec encode_message_type(message_type()) -> 0..255.
encode_message_type(hello) -> 0;
encode_message_type(bye) -> 1;
encode_message_type(ping) -> 2;
encode_message_type(pong) -> 3;
encode_message_type(error) -> 4;
encode_message_type(data) -> 5.

-spec encode_error_code(error_code()) -> 0..255.
encode_error_code(unspecified_error) -> 0;
encode_error_code(io_error) -> 1;
encode_error_code(unspecified_timeout) -> 2;
encode_error_code(protocol_error) -> 3;
encode_error_code(extension_error) -> 4.

-spec encode_extensions([extension()]) -> iodata().
encode_extensions(Extensions) ->
  encode_extensions(Extensions, []).

-spec encode_extensions([extension()], iodata()) -> iodata().
encode_extensions([], Acc) ->
  lists:reverse(Acc);
encode_extensions([Extension], Acc) ->
  [encode_extension(Extension, 0) | Acc];
encode_extensions([Extension | Extensions], Acc) ->
  encode_extensions(Extensions, [encode_extension(Extension, 1) | Acc]).

-spec encode_extension(extension(), More :: 0..1) -> iodata().
encode_extension({request_response, #{type := Type, id := Id}}, More) ->
  R = case Type of
        request -> 1;
        response -> 0
      end,
  Content = <<R:1, 0:31, Id:64>>,
  encode_extension_block(0, More, Content);
encode_extension({compression, #{scheme := Scheme}}, More) ->
  Content = <<(encode_compression_scheme(Scheme)):8, 0:24>>,
  encode_extension_block(1, More, Content).

-spec encode_extension_block(extension_id(), More :: 0..1,
                             Content :: binary()) -> iodata().
encode_extension_block(Id, More, Content) ->
  Data = [<<Id:8, More:1, 0:23>>, Content],
  case iolist_size(Data) of
    Size when Size < 4294967296 ->
      [<<Size:32>>, Data];
    _ ->
      error(extension_too_large)
  end.

-spec encode_body(message_body()) -> iodata().
encode_body(#{version := Version}) ->
  <<Version:8, 0:24>>;
encode_body(#{code := Code, description := Description}) ->
  [<<(encode_error_code(Code)):8, 0:24>>, encode_string(Description)];
encode_body(Data) when is_list(Data); is_binary(Data) ->
  Data.

-spec encode_compression_scheme(compression_scheme()) -> 0..255.
encode_compression_scheme(identity) -> 0;
encode_compression_scheme(gzip) -> 1.

-spec decode_message(binary()) -> {ok, message()} | {error, term()}.
decode_message(Data) ->
  try
    {ok, decode_header(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec decode_header(binary()) -> message().
decode_header(<<Type:8, E:1, _:23, Rest/binary>>) ->
  Message = #{type => decode_message_type(Type)},
  case E of
    0 ->
      decode_body(Rest, Message);
    1 ->
      decode_extensions(Rest, Message)
  end;
decode_header(_) ->
  throw({error, invalid_format}).

-spec decode_message_type(byte()) -> message_type().
decode_message_type(0) -> hello;
decode_message_type(1) -> bye;
decode_message_type(2) -> ping;
decode_message_type(3) -> pong;
decode_message_type(4) -> error;
decode_message_type(5) -> data;
decode_message_type(Type) ->
  throw({error, {unknown_message_type, Type}}).

-spec decode_extensions(binary(), message()) -> message().
decode_extensions(<<Size:32, Block:Size/binary, Rest/binary>>, Message) ->
  case Block of
    <<Id:8, More:1, _:23, Content/binary>> ->
      Message2 = decode_extension(Id, Content, Message),
      case More of
        1 ->
          decode_extensions(Rest, Message2);
        0 ->
          decode_body(Rest, Message2)
      end;
    _ ->
      throw({error, invalid_format})
  end;
decode_extensions(_, _) ->
  throw({error, invalid_format}).

-spec decode_extension(0..255, binary(), message()) -> message().
decode_extension(0, Content, Message) ->
  case Content of
    <<R:1, _:31, Id:64>> ->
      Type = case R of
               1 -> request;
               0 -> response
             end,
      Extension = {request_response, #{type => Type, id => Id}},
      add_extension(Extension, Message);
    _ ->
      throw({error, {invalid_extension, 0}})
  end;
decode_extension(1, Content, Message) ->
  case Content of
    <<Scheme:8, _:24>> ->
      Extension = {compression, #{scheme => decode_compression_scheme(Scheme)}},
      add_extension(Extension, Message);
    _ ->
      throw({error, {invalid_extension, 1}})
  end;
decode_extension(Id, _, _) ->
  throw({error, {unknown_extension, Id}}).

-spec decode_compression_scheme(0..255) -> compression_scheme().
decode_compression_scheme(0) -> identity;
decode_compression_scheme(1) -> gzip;
decode_compression_scheme(Scheme) ->
  throw({error, {unknown_compression_scheme, Scheme}}).

-spec decode_body(binary(), message()) -> message().
decode_body(Data, Message = #{type := hello}) ->
  case Data of
    <<Version:8, _:24>> ->
      Message#{body => #{version => Version}};
    _ ->
      throw({error, invalid_body})
  end;
decode_body(_, Message = #{type := bye}) ->
  Message;
decode_body(_, Message = #{type := ping}) ->
  Message;
decode_body(_, Message = #{type := pong}) ->
  Message;
decode_body(Data, Message = #{type := error}) ->
  case Data of
    <<Code:8, _:24, DescriptionData/binary>> ->
      Description = decode_string(DescriptionData),
      Message#{body => #{code => decode_error_code(Code),
                         description => Description}};
    _ ->
      throw({error, invalid_body})
  end;
decode_body(Data, Message = #{type := data}) ->
  Message#{body => Data}.

-spec decode_error_code(0..255) -> error_code().
decode_error_code(0) -> unspecified_error;
decode_error_code(1) -> io_error;
decode_error_code(2) -> unspecified_timeout;
decode_error_code(3) -> protocol_error;
decode_error_code(4) -> extension_error;
decode_error_code(Code) ->
  throw({error, {unknown_error_code, Code}}).

-spec add_extension(extension(), message()) -> message().
add_extension(Extension, Message) ->
  Extensions = maps:get(extensions, Message, []),
  Message#{extensions => [Extension | Extensions]}.

-spec encode_string(binary() | string()) -> binary().
encode_string(String) ->
  case unicode:characters_to_binary(String) of
    Data when is_binary(Data) ->
      case iolist_size(Data) of
        Size when Size < 65536 ->
          <<Size:16, Data/binary>>;
        _ ->
          error(string_too_long)
      end;
    {error, Datum, _} ->
      error({invalid_character_data, Datum});
    {incomplete, Datum, _} ->
      error({incomplete_character_data, Datum})
  end.

-spec decode_string(binary()) -> binary().
decode_string(<<Size:16, Data:Size/binary>>) ->
  case unicode:characters_to_binary(Data) of
    String when is_binary(String) ->
      String;
    {error, Datum, _} ->
      throw({error, {invalid_character_data, Datum}});
    {incomplete, Datum, _} ->
      throw({error, {incomplete_character_data, Datum}})
  end;
decode_string(_) ->
  throw({error, invalid_string}).
