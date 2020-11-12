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

-module(emp).

-export([default_port/0,
         success_response/0, success_response/1,
         failure_response/2, failure_response/3, failure_response/4,
         send_message/2, send_request/3]).

-export_type([gen_server_name/0, gen_server_ref/0, gen_server_call_tag/0,
              client_id/0, server_id/0,
              sender/0,
              request/0, request_id/0,
              response/0, response_status/0,
              op_name/0, op/0, op_table/0]).

-type gen_server_name() :: {local, term()}
                         | {global, term()}
                         | {via, atom(), term()}.

-type gen_server_ref() :: term()
                        | {term(), atom()}
                        | {global, term()}
                        | {via, atom(), term()}
                        | pid().

-type gen_server_call_tag() :: {pid(), term()}.

-type client_id() :: atom().
-type server_id() :: atom().

-type sender() :: {client, client_id()}
                | {connection, pid()}.

-type request() :: #{id => request_id(),
                     op := op_name(),
                     data := json:value()}.

-type request_id() :: 1..18446744073709551615.

-type response() :: #{id => request_id(),
                      status := response_status(),
                      error_code => atom() | binary(),
                      description => binary(),
                      data => json:value()}.
-type response_status() :: success | failure.

-type op_name() :: atom().
-type op() :: #{input := jsv:definition(),
                output => jsv:definition(),
                error => jsv:definition()}.
-type op_table() :: #{binary() := op()}.

-spec default_port() -> inet:port_number().
default_port() ->
  5040.

-spec success_response() -> emp:response().
success_response() ->
  #{status => success}.

-spec success_response(json:value()) -> emp:response().
success_response(Data) ->
  #{status => success,
    data => Data}.

-spec failure_response(ErrorCode, Description) -> emp:response() when
    ErrorCode :: atom(),
    Description :: string() | binary().
failure_response(ErrorCode, Description) ->
  #{status => failure,
    error_code => ErrorCode,
    description => unicode:characters_to_binary(Description)}.

-spec failure_response(ErrorCode, io:format() | Description,
                       [term()] | json:value()) ->
        emp:response() when
    ErrorCode :: atom(),
    Description :: string() | binary().
failure_response(ErrorCode, Description, Data) when is_map(Data) ->
  #{status => failure,
    error_code => ErrorCode,
    description => unicode:characters_to_binary(Description),
    data => Data};
failure_response(ErrorCode, Format, Args) when is_list(Args) ->
  DescriptionData = io_lib:format(Format, Args),
  #{status => failure,
    error_code => ErrorCode,
    description => iolist_to_binary(DescriptionData)}.

-spec failure_response(ErrorCode, io:format(), [term()], json:value()) ->
        emp:response() when
    ErrorCode :: atom().
failure_response(ErrorCode, Format, Args, Data) ->
  DescriptionData = io_lib:format(Format, Args),
  #{status => failure,
    error_code => ErrorCode,
    description => iolist_to_binary(DescriptionData),
    data => Data}.

-spec send_message(sender(), emp_proto:message()) -> ok | {error, term()}.
send_message({client, ClientId}, Message) ->
  ClientRef = emp_client:process_name(ClientId),
  emp_client:send_message(ClientRef, Message);
send_message({connection, Pid}, Message) ->
  emp_connection:send_message(Pid, Message).

-spec send_request(sender(), op_name(), json:value()) ->
        {ok, emp:response()} | {error, term()}.
send_request({client, ClientId}, Op, Data) ->
  Request = #{op => Op, data => Data},
  ClientRef = emp_client:process_name(ClientId),
  emp_client:send_request(ClientRef, Request);
send_request({connection, Pid}, Op, Data) ->
  Request = #{op => Op, data => Data},
  emp_connection:send_request(Pid, Request).
