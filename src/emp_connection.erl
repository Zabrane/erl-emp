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

-module(emp_connection).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/3, send_message/2, send_request/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([handler/0, handler_request/0, options/0]).

-type handler() :: emp:gen_server_ref().
-type handler_request() :: {emp_data, iodata()}
                         | {emp_request, iodata()}.

-type options() :: #{ping_interval => pos_integer(),
                     handler => handler(),
                     op_table_name => emp_ops:op_table_name()}.

-type state() :: #{options := options(),
                   socket => emp_socket:socket(),
                   address := inet:ip_address(),
                   port := inet:port_number(),
                   pending_requests := queue:queue(pending_request()),
                   next_request_id := emp:request_id(),
                   op_table_name => emp_ops:op_table_name()}.

-type pending_request() :: #{request := emp:request(),
                             source := emp:gen_server_call_tag()}.

-spec start_link(Address, Port, options()) -> Result when
    Address :: inet:ip_address(),
    Port :: inet:port_number(),
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Address, Port, Options) ->
  gen_server:start_link(?MODULE, [Address, Port, Options], []).

-spec send_message(pid(), emp_proto:message()) -> ok | {error, term()}.
send_message(Pid, Message) ->
  gen_server:call(Pid, {send_message, Message}, infinity).

-spec send_request(pid(), emp:request()) -> {ok, iodata()} | {error, term()}.
send_request(Pid, Request) ->
  gen_server:call(Pid, {send_request, Request}, infinity).

-spec init(list()) -> {ok, state()}.
init([Address, Port, Options]) ->
  logger:update_process_metadata(#{domain => [emp, connection]}),
  OpTableName = maps:get(op_table_name, Options,
                         emp_ops:default_op_table_name()),
  State = #{options => Options,
            address => Address,
            port => Port,
            pending_requests => queue:new(),
            next_request_id => 1,
            op_table_name => OpTableName},
  {ok, State}.

terminate(_Reason, #{socket := Socket}) ->
  emp_socket:close(Socket),
  ok;
terminate(_Reason, _State) ->
  ok.

handle_call({send_message, Message}, _From, State) ->
  try
    do_send_message(Message, State),
    {reply, ok, State}
  catch
    throw:{error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({send_request, Request}, From,
            State = #{pending_requests := PendingRequests,
                      next_request_id := Id}) ->
  try
    Request2 = Request#{id => Id},
    Message = emp_proto:request_message(Request2),
    do_send_message(Message, State),
    PendingRequest = #{request => Request2, source => From},
    State2 = State#{pending_requests => queue:in(PendingRequest,
                                                 PendingRequests),
                    next_request_id => Id+1},
    {noreply, State2}
  catch
    throw:{error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({socket, Socket}, State = #{options := Options}) ->
  State2 = State#{socket => Socket},
  case handshake(State2) of
    {ok, State3} ->
      ok = emp_socket:setopts(Socket, [{active, 1}]),
      PingInterval = maps:get(ping_interval, Options, 10_000),
      {ok, _} = timer:send_interval(PingInterval, self(), send_ping),
      {noreply, State3};
    {error, Reason} ->
      ?LOG_ERROR("handshake failed: ~p", [Reason]),
      {stop, {error, Reason}, State2}
  end;

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(send_ping, State) ->
  do_send_message(emp_proto:ping_message(), State),
  {noreply, State};

handle_info({Event, _}, _State) when
    Event =:= tcp_closed; Event =:= ssl_closed ->
  ?LOG_INFO("connection closed"),
  exit(normal);

handle_info({Event, _}, State) when
    Event =:= tcp_passive; Event =:= ssl_passive ->
  {noreply, State};

handle_info({Event, _, Data}, State = #{socket := Socket}) when
    Event =:= tcp; Event =:= ssl ->
  case emp_proto:decode_message(Data) of
    {ok, Message} ->
      ok = emp_socket:setopts(Socket, [{active, 1}]),
      case handle_message(Message, State) of
        {ok, State2} ->
          {noreply, State2};
        {error, Reason} ->
          {stop, Reason, State}
      end;
    {error, Reason} ->
      send_error(protocol_error, "invalid data: ~p", [Reason], State),
      {stop, {invalid_data, Reason}, State}
  end;

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec handshake(state()) -> {ok, state()} | {error, term()}.
handshake(State = #{socket := Socket}) ->
  do_send_message(emp_proto:hello_message(), State),
  case emp_socket:recv(Socket, 0, 5000) of
    {ok, Data} ->
      CurrentVersion = emp_proto:version(),
      case emp_proto:decode_message(Data) of
        {ok, #{type := hello, body := #{version := Version}}} when
            Version =< CurrentVersion ->
          {ok, State};
        {ok, #{type := hello, body := #{version := Version}}} ->
          send_error(protocol_error, "unsupported version", State),
          {error, {unsupported_version, Version}};
        {ok, Message} ->
          send_error(protocol_error, "unexpected message", State),
          {error, {unexpected_message, Message}};
        {error, Reason} ->
          send_error(protocol_error, "invalid data: ~p", [Reason], State),
          {error, {invalid_data, Reason}}
      end;
    {error, timeout} ->
      {error, no_handshake_response};
    {error, Reason} ->
      {error, {recv, Reason}}
  end.

-spec do_send_message(emp_proto:message(), state()) -> ok.
do_send_message(Message, #{socket := Socket}) ->
  Data = emp_proto:encode_message(Message),
  case emp_socket:send(Socket, Data) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, {send, Reason}})
  end.

-spec send_error(emp_proto:error_code(), binary() | string(), state()) -> ok.
send_error(Code, Description, State) ->
  Message = emp_proto:error_message(Code, Description),
  do_send_message(Message, State).

-spec send_error(emp_proto:error_code(), io:format(), [term()], state()) -> ok.
send_error(Code, Format, Args, State) ->
  Message = emp_proto:error_message(Code, Format, Args),
  do_send_message(Message, State).

-spec handle_message(emp_proto:message(), state()) ->
        {ok, state()} | {error, term()}.
handle_message(#{type := ping}, State) ->
  do_send_message(emp_proto:pong_message(), State),
  {ok, State};
handle_message(#{type := pong}, State) ->
  {ok, State};
handle_message(#{type := error,
                 body := #{code := Code, description := Description}},
               _State) ->
  ?LOG_WARNING("peer error (~p): ~ts", [Code, Description]),
  {error, normal};
handle_message(Message = #{type := request}, State) ->
  handle_request_message(Message, State);
handle_message(Message = #{type := response}, State) ->
  handle_response_message(Message, State);
handle_message(Message, _State) ->
  error({unexpected_message, Message}).

-spec handle_request_message(emp_proto:message(), state()) ->
        {ok, state()} | {error, term()}.
handle_request_message(Message, State = #{op_table_name := OpTableName}) ->
  case emp_request:parse(Message, OpTableName) of
    {ok, Request} ->
      handle_request(Request, State);
    {error, Reason = {invalid_data, Error}} ->
      ErrorString = io_lib:format("~p", [Error]), % TODO JSON error formatting
      send_error(invalid_request, ErrorString, State),
      {error, {invalid_request, Reason}};
    {error, Reason = {invalid_value, Errors}} ->
      ErrorString = emp_jsv:format_value_errors(Errors),
      send_error(invalid_request, ErrorString, State),
      {error, {invalid_request, Reason}};
    {error, Reason = {invalid_op, OpString}} ->
      send_error(invalid_request, "invalid op \"~ts\"", [OpString], State),
      {error, {invalid_request, Reason}}
  end.

-spec handle_request(emp:request(), state()) -> {ok, state()} | {error, term()}.
handle_request(Request = #{id := Id}, State) ->
  case execute_request(Request, State) of
    {ok, Response0} ->
      Response = Response0#{id => Id},
      ResponseMessage = emp_proto:response_message(Response),
      do_send_message(ResponseMessage, State),
      {ok, State};
    {error, Reason} ->
      {error, Reason}
  end.

-spec execute_request(emp:request(), state()) ->
        {ok, emp:response()} | {error, term()}.
execute_request(Request = #{op := <<$$, _/binary>>}, State) ->
  execute_internal_request(Request, State);
execute_request(Request, State) ->
  call_handler({emp_request, Request}, State).

-spec execute_internal_request(emp:request(), state()) ->
        {ok, emp:response()} | {error, term()}.

execute_internal_request(#{op := <<"$echo">>, data := Data}, _State) ->
  {ok, emp:success_response(Data)};

execute_internal_request(#{op := <<"$get_op">>,
                           data := #{op_name := OpName}},
                         #{op_table_name := OpTableName}) ->
  case emp_ops:find_op(OpName, OpTableName) of
    {ok, Op} ->
      OpValue = emp_ops:serialize_op(OpName, Op),
      {ok, emp:success_response(#{op => OpValue})};
    error ->
      {ok, emp:failure_response(unknown_op, "unknown op ~p", [OpName])}
  end;

execute_internal_request(#{op := <<"$list_ops">>},
                         #{op_table_name := OpTableName}) ->
  Ops = emp_ops:all_ops(OpTableName),
  OpsValue = maps:fold(fun (OpName, Op, Acc) ->
                           [emp_ops:serialize_op(OpName, Op) | Acc]
                       end, [], Ops),
  {ok, emp:success_response(#{ops => OpsValue})};

execute_internal_request(#{op := OpName}, _State) ->
  ?LOG_WARNING("unknown op ~p", [OpName]),
  Response = emp:failure_response(invalid_op, "invalid op ~p", [OpName]),
  {ok, Response}.

-spec handle_response_message(emp_proto:message(), state()) ->
        {ok, state()} | {error, term()}.
handle_response_message(Message = #{body := #{id := Id}},
                        State = #{pending_requests := PendingRequests,
                                  op_table_name := OpTableName}) ->
  case queue:out(PendingRequests) of
    {{value, #{request := #{id := Id, op := OpName},
               source := Source}},
     PendingRequests2} ->
      State2 = State#{pending_requests => PendingRequests2},
      case emp_response:parse(Message, OpName, OpTableName) of
        {ok, Response} ->
          gen_server:reply(Source, {ok, Response}),
          State2 = State#{pending_requests => PendingRequests2},
          {ok, State2};
        {error, Reason = {invalid_data, Error}} ->
          ErrorString = io_lib:format("~p", [Error]), % TODO JSON error formatting
          send_error(invalid_response, ErrorString, State2),
          {error, {invalid_response, Reason}};
        {error, Reason = {invalid_value, Errors}} ->
          ErrorString = emp_jsv:format_value_errors(Errors),
          send_error(invalid_response, ErrorString, State2),
          {error, {invalid_response, Reason}}
      end;
    {{value, _}, _} ->
      send_error(invalid_response, "invalid request id ~b", [Id], State),
      {error, {invalid_request_id, Id}};
    {empty, _} ->
      send_error(invalid_response, "invalid request id ~b", [Id], State),
      {error, {invalid_request_id, Id}}
  end.

-spec call_handler(term(), state()) -> {ok, term()} | {error, term()}.
call_handler(Call, State = #{options := Options}) ->
  case maps:find(handler, Options) of
    {ok, Handler} ->
      try
        {ok, gen_server:call(Handler, Call, infinity)}
      catch
        exit:{noproc, _MFA} ->
          send_error(service_unavailable, "message handler (~p) down",
                     [Handler], State),
          {error, normal}
      end;
    error ->
      send_error(service_unavailable, "missing message handler", State),
      {error, normal}
  end.
