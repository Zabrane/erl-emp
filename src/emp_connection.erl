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
                     ops => emp:op_table()}.

-type state() :: #{options := options(),
                   socket => emp_socket:socket(),
                   address := inet:ip_address(),
                   port := inet:port_number(),
                   pending_requests := queue:queue(pending_request()),
                   next_request_id := emp:request_id(),
                   ops := emp:op_table()}.

-type pending_request() :: #{id := emp:request_id(),
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
  Ops = maps:get(ops, Options, emp_ops:default_ops()),
  State = #{options => Options,
            address => Address,
            port => Port,
            pending_requests => queue:new(),
            next_request_id => 1,
            ops => Ops},
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
    PendingRequest = #{id => Id, source => From},
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
      ?LOG_DEBUG("handshake finished"),
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
  handle_response(Message, State);
handle_message(Message, _State) ->
  error({unexpected_message, Message}).

-spec handle_request_message(emp_proto:message(), state()) ->
        {ok, state()} | {error, term()}.
handle_request_message(Message, State = #{ops := Ops}) ->
  case emp_request:parse(Message, Ops) of
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
  case call_handler({emp_request, Request}, State) of
    {ok, ResponseData} ->
      Response = emp_proto:response_message(Id, ResponseData),
      do_send_message(Response, State),
      {ok, State};
    {error, Reason} ->
      {error, Reason}
  end.

-spec handle_response(emp_proto:message(), state()) ->
        {ok, state()} | {error, term()}.
handle_response(#{type := response,
                  body := #{id := Id, data := Data}},
                State = #{pending_requests := PendingRequests}) ->
  case queue:out(PendingRequests) of
    {{value, #{id := Id, source := Source}},
     PendingRequests2} ->
      gen_server:reply(Source, Data),
      State2 = State#{pending_requests => PendingRequests2},
      {ok, State2};
    {{value, _}, _} ->
      send_error(invalid_request_id, "invalid request id ~b", [Id], State),
      {error, {invalid_request_id, Id}};
    {empty, _} ->
      send_error(invalid_request_id, "invalid request id ~b", [Id], State),
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
          send_error(service_unavailable, "message handler down", State),
          {error, normal}
      end;
    error ->
      send_error(service_unavailable, "missing message handler", State),
      {error, normal}
  end.
