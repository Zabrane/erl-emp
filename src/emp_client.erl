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

-module(emp_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/2, stop/1,
         send_message/2, send_request/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([client_name/0, client_ref/0, options/0]).

-type client_name() :: emp:gen_server_name().
-type client_ref() :: emp:gen_server_ref().

-type options() :: #{host => binary(),
                     port => inet:port_number(),
                     transport => emp_socket:transport(),
                     connection_timeout => timeout(),
                     socket_options => emp_socket:connect_options(),
                     connection_options => emp_connection:options()}.

-type state() :: #{options := options(),
                   backoff := backoff:backoff(),
                   connection_pid => pid()}.

-spec process_name(emp:client_id()) -> atom().
process_name(Id) ->
  Name = <<"emp_client_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(client_name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec stop(client_ref()) -> ok.
stop(Name) ->
  gen_server:stop(Name).

-spec send_message(client_ref(), emp_proto:message()) -> ok | {error, term()}.
send_message(Ref, Message) ->
  gen_server:call(Ref, {send_message, Message}, infinity).

-spec send_request(client_ref(), emp:request()) ->
        {ok, emp:response()} | {error, term()}.
send_request(Ref, Request) ->
  gen_server:call(Ref, {send_request, Request}, infinity).

init([Options]) ->
  logger:update_process_metadata(#{domain => [emp, client]}),
  process_flag(trap_exit, true),
  Backoff = backoff:type(backoff:init(1000, 60000), jitter),
  State = #{options => Options,
            backoff => Backoff},
  self() ! connect,
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call({send_message, Data}, _From, State) ->
  case
    call_connection(fun (Pid) ->
                        emp_connection:send_message(Pid, Data)
                    end, State)
  of
    {ok, Result} ->
      {reply, Result, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({send_request, Request}, _From, State) ->
  case
    call_connection(fun (Pid) ->
                        emp_connection:send_request(Pid, Request)
                    end, State)
  of
    {ok, Result} ->
      {reply, Result, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(connect, State = #{backoff := Backoff}) ->
  case connect(State) of
    {ok, State2} ->
      {_, Backoff2} = backoff:succeed(Backoff),
      {noreply, State2#{backoff => Backoff2}};
    {error, _} ->
      {_, Backoff2} = backoff:fail(Backoff),
      schedule_connection(Backoff2),
      {noreply, State#{backoff => Backoff2}}
  end;

handle_info({'EXIT', _Pid, normal}, State = #{backoff := Backoff}) ->
  schedule_connection(Backoff),
  {noreply, maps:remove(connection_pid, State)};
handle_info({'EXIT', Pid, Reason}, State = #{backoff := Backoff}) ->
  ?LOG_WARNING("connection ~p exited (~p)", [Pid, Reason]),
  schedule_connection(Backoff),
  {noreply, maps:remove(connection_pid, State)};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec schedule_connection(backoff:backoff()) -> ok.
schedule_connection(Backoff) ->
  {ok, _} = timer:send_after(backoff:get(Backoff), self(), connect),
  ok.

-spec call_connection(fun((pid()) -> term()), state()) ->
        {ok, Reply :: term()} | {error, term()}.
call_connection(Fun, State) ->
  try
    case maps:find(connection_pid, State) of
      {ok, Pid} ->
        {ok, Fun(Pid)};
      error ->
        {error, connection_unavailable}
    end
  catch
    exit:{Reason, _MFA} ->
      {error, {connection_failure, Reason}}
  end.

-spec connect(state()) -> {ok, state()} | {error, term()}.
connect(State = #{options := Options}) ->
  Transport = maps:get(transport, Options, tcp),
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, emp:default_port()),
  Timeout = maps:get(connection_timeout, Options, 5000),
  SocketOptions = [{active, false},
                   {mode, binary},
                   {packet, 4}] ++
    maps:get(socket_options, Options, []),
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  Host2 = unicode:characters_to_list(Host),
  case emp_socket:connect(Transport, Host2, Port, SocketOptions, Timeout) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      case emp_socket:peername(Socket) of
        {ok, {PeerAddress, PeerPort}} ->
          Pid = spawn_connection(Socket, PeerAddress, PeerPort, State),
          {ok, State#{connection_pid => Pid}};
        {error, Reason} ->
          ?LOG_ERROR("cannot obtain peer address and port: ~p", Reason),
          emp_socket:close(Socket),
          {error, {peername, Reason}}
      end;
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, {connect, Reason}}
  end.

-spec spawn_connection(emp_socket:socket(),
                       inet:ip_address(), inet:port_number(),
                       state()) ->
        pid().
spawn_connection(Socket, Address, Port, #{options := Options}) ->
  ConnOptions = maps:get(connection_options, Options, #{}),
  {ok, Pid} = emp_connection:start_link(Address, Port, ConnOptions),
  ok = emp_socket:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {socket, Socket}),
  Pid.
