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

-export([version/0,
         hello_message/0, hello_message/1,
         bye_message/0, ping_message/0, pong_message/0,
         error_message/2, error_message/3, data_message/1,
         request_message/2, response_message/2,
         encode_message/1, decode_message/1,
         encode_string/1, decode_string/1]).

-export_type([message_type/0, message/0,
              message_body/0,
              hello_message_body/0, error_message_body/0, data_message_body/0,
              request_message_body/0, response_message_body/0,
              version/0, error_code/0,
              request_id/0]).

-type message_type() :: hello | bye | ping | pong | error
                      | data | request | response.

-type message() :: #{type := message_type(),
                     body => message_body()}.

-type message_body() :: hello_message_body()
                      | error_message_body()
                      | data_message_body()
                      | request_message_body()
                      | response_message_body().

-type hello_message_body() :: #{version := version()}.

-type error_message_body() :: #{code := error_code(),
                                description := binary()}.

-type data_message_body() :: #{data := iodata()}.

-type request_message_body() :: #{id := request_id(),
                                  data := iodata()}.

-type response_message_body() :: #{id := request_id(),
                                   data := iodata()}.

-type version() :: 1..255.

-type error_code() :: internal_error
                    | protocol_error
                    | invalid_request_id.

-type request_id() :: 0..18446744073709551615.

-spec version() -> version().
version() ->
  1.

-spec hello_message() -> message().
hello_message() ->
  #{type => hello, body => #{version => version()}}.

-spec hello_message(version()) -> message().
hello_message(Version) ->
  #{type => hello, body => #{version => Version}}.

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
  #{type => data, body => #{data => Body}}.

-spec request_message(request_id(), Body :: iodata()) -> message().
request_message(Id, Body) ->
  #{type => request, body => #{id => Id, data => Body}}.

-spec response_message(request_id(), Body :: iodata()) -> message().
response_message(Id, Body) ->
  #{type => response, body => #{id => Id, data => Body}}.

-spec encode_message(message()) -> iodata().
encode_message(Message = #{type := Type}) ->
  BodyData = case maps:find(body, Message) of
               {ok, Body} -> encode_body(Type, Body);
               error -> []
             end,
  Header = <<(encode_message_type(Type)):8, 0:24>>,
  [Header, BodyData].

-spec encode_message_type(message_type()) -> 0..255.
encode_message_type(hello) -> 0;
encode_message_type(bye) -> 1;
encode_message_type(ping) -> 2;
encode_message_type(pong) -> 3;
encode_message_type(error) -> 4;
encode_message_type(data) -> 5;
encode_message_type(request) -> 6;
encode_message_type(response) -> 7.

-spec encode_error_code(error_code()) -> 0..255.
encode_error_code(internal_error) -> 0;
encode_error_code(protocol_error) -> 1;
encode_error_code(invalid_request_id) -> 2.

-spec encode_body(message_type(), message_body()) -> iodata().
encode_body(hello, #{version := Version}) ->
  <<Version:8, 0:24>>;
encode_body(error, #{code := Code, description := Description}) ->
  [<<(encode_error_code(Code)):8, 0:24>>, encode_string(Description)];
encode_body(data, #{data := Data}) ->
  Data;
encode_body(request, #{id := Id, data := Data}) ->
  [<<Id:64>>, Data];
encode_body(response, #{id := Id, data := Data}) ->
  [<<Id:64>>, Data].

-spec decode_message(binary()) -> {ok, message()} | {error, term()}.
decode_message(Data) ->
  try
    {ok, decode_header(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec decode_header(binary()) -> message().
decode_header(<<Type:8, _:24, Rest/binary>>) ->
  Message = #{type => decode_message_type(Type)},
  decode_body(Rest, Message);
decode_header(_) ->
  throw({error, invalid_format}).

-spec decode_message_type(byte()) -> message_type().
decode_message_type(0) -> hello;
decode_message_type(1) -> bye;
decode_message_type(2) -> ping;
decode_message_type(3) -> pong;
decode_message_type(4) -> error;
decode_message_type(5) -> data;
decode_message_type(6) -> request;
decode_message_type(7) -> response;
decode_message_type(Type) ->
  throw({error, {unknown_message_type, Type}}).

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
  Message#{body => #{data => Data}};
decode_body(Data, Message = #{type := request}) ->
  case Data of
    <<Id:64, Data2/binary>> ->
      Message#{body => #{id => Id, data => Data2}};
    _ ->
      throw({error, invalid_body})
  end;
decode_body(Data, Message = #{type := response}) ->
  case Data of
    <<Id:64, Data2/binary>> ->
      Message#{body => #{id => Id, data => Data2}};
    _ ->
      throw({error, invalid_body})
  end.

-spec decode_error_code(0..255) -> error_code().
decode_error_code(0) -> internal_error;
decode_error_code(1) -> protocol_error;
decode_error_code(2) -> invalid_request_id;
decode_error_code(Code) ->
  throw({error, {unknown_error_code, Code}}).

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
