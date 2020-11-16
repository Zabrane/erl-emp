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
         failure_response/1, failure_response/2, failure_response/3,
         unhandled_op_failure_response/1,
         service_unavailable_failure_response/0,
         internal_failure_response/1,
         install_op_catalog/2, uninstall_op_catalog/1,
         send_message/2, send_request/2, send_request/3]).

-export_type([gen_server_name/0, gen_server_ref/0, gen_server_call_tag/0,
              client_id/0, server_id/0,
              sender/0,
              request/0, request_id/0,
              response/0, response_status/0,
              request_result/0,
              op_name/0, op/0, op_catalog/0, op_catalog_name/0,
              op_table_name/0]).

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
                      description => binary(),
                      data => json:value()}.
-type response_status() :: success | failure.

-type request_result() :: {ok, json:value()}
                        | {error, {request_failure,
                                   Description :: binary(),
                                   json:value()}}.

-type op_name() :: binary().
-type op() :: #{input := jsv:definition(),
                output => jsv:definition(),
                error => jsv:definition()}.
-type op_catalog() :: #{op_name() := op()}.
-type op_catalog_name() :: atom().
-type op_table_name() :: atom().

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

-spec failure_response(Description) -> emp:response() when
    Description :: string() | binary().
failure_response(Description) ->
  #{status => failure,
    description => unicode:characters_to_binary(Description)}.

-spec failure_response(io:format() | Description, [term()] | json:value()) ->
        emp:response() when
    Description :: string() | binary().
failure_response(Description, Data) when is_map(Data) ->
  #{status => failure,
    description => unicode:characters_to_binary(Description),
    data => Data};
failure_response(Format, Args) when is_list(Args) ->
  DescriptionData = io_lib:format(Format, Args),
  #{status => failure,
    description => iolist_to_binary(DescriptionData)}.

-spec failure_response(io:format(), [term()], json:value()) -> emp:response().
failure_response(Format, Args, Data) ->
  DescriptionData = io_lib:format(Format, Args),
  #{status => failure,
    description => iolist_to_binary(DescriptionData),
    data => Data}.

-spec unhandled_op_failure_response(op_name()) -> emp:response().
unhandled_op_failure_response(OpName) ->
  failure_response("unhandled op \"~ts\"", [OpName],
                   #{error => <<"unhandled_op">>, op => OpName}).

-spec service_unavailable_failure_response() -> emp:response().
service_unavailable_failure_response() ->
  failure_response("service unavailable",
                   #{error => <<"service_unavailable">>}).

-spec internal_failure_response(term()) -> emp:response().
internal_failure_response(Reason) ->
  failure_response("internal error: ~p", [Reason],
                   #{error => <<"internal">>}).

-spec install_op_catalog(op_catalog_name(), op_catalog()) -> op_table_name().
install_op_catalog(Name, Catalog) ->
  Catalog2 = maps:merge(Catalog, emp_ops:internal_op_catalog()),
  emp_op_catalog_registry:install_catalog(Name, Catalog2).

-spec uninstall_op_catalog(op_catalog_name()) -> ok.
uninstall_op_catalog(Name) ->
  emp_op_catalog_registry:uninstall_catalog(Name).

-spec send_message(sender(), emp_proto:message()) -> ok | {error, term()}.
send_message({client, ClientId}, Message) ->
  ClientRef = emp_client:process_name(ClientId),
  emp_client:send_message(ClientRef, Message);
send_message({connection, Pid}, Message) ->
  emp_connection:send_message(Pid, Message).

-spec send_request(sender(), op_name()) -> request_result().
send_request(Sender, Op) ->
  send_request(Sender, Op, #{}).

-spec send_request(sender(), op_name(), json:value()) -> request_result().
send_request(Sender, Op, Data) ->
  Request = #{op => Op, data => Data},
  Result = case Sender of
             {client, ClientId} ->
               ClientRef = emp_client:process_name(ClientId),
               emp_client:send_request(ClientRef, Request);
             {connection, Pid} ->
               emp_connection:send_request(Pid, Request)
           end,
  case Result of
    {ok, #{status := success, data := ResData}} ->
      {ok, ResData};
    {ok, #{status := failure, description := Description, data := ResData}} ->
      {error, {request_failure, Description, ResData}};
    {error, Reason} ->
      {error, Reason}
  end.
