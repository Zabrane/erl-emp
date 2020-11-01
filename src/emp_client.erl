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

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([client_name/0, client_ref/0, options/0]).

-type client_name() :: emp:gen_server_name().
-type client_ref() :: emp:gen_server_ref().

-type options() :: #{host => binary(),
                     port => inet:port_number(),
                     connection_timeout => timeout(),
                     transport => emp:transport()}.

-type state() :: #{options := options(),
                   transport := emp:transport(),
                   backoff := backoff:backoff(),
                   socket => inet:socket() | ssl:sslsocket()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

init([Options]) ->
  logger:update_process_metadata(#{domain => [emp, client]}),
  Transport = maps:get(transport, Options, tcp),
  Backoff = backoff:type(backoff:init(1000, 60000), jitter),
  State = #{options => Options,
            transport => Transport,
            backoff => Backoff},
  self() ! connect,
  {ok, State}.

terminate(Reason, State = #{transport := tcp, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  gen_tcp:close(Socket),
  terminate(Reason, State);
terminate(Reason, State = #{transport := tls, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  ssl:close(Socket),
  terminate(Reason, State);
terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

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

handle_info({Event, _}, State = #{backoff := Backoff}) when
    Event =:= tcp_closed; Event =:= ssl_closed ->
  ?LOG_INFO("connection closed"),
  schedule_connection(Backoff),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec schedule_connection(backoff:backoff()) -> ok.
schedule_connection(Backoff) ->
  timer:send_after(backoff:get(Backoff), self(), connect),
  ok.

-spec connect(state()) -> {ok, state()} | {error, term()}.
connect(State = #{options := Options}) ->
  case maps:get(transport, Options, tcp) of
    tcp ->
      connect_tcp(State);
    tls ->
      connect_tls(State)
  end.

-spec connect_tcp(state()) -> {ok, state()} | {error, term()}.
connect_tcp(State = #{options := Options}) ->
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, emp:default_port()),
  Timeout = maps:get(connection_timeout, Options, 5000),
  TCPOptions = [{mode, binary}],
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  HostString = unicode:characters_to_list(Host),
  case gen_tcp:connect(HostString, Port, TCPOptions, Timeout) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      State2 = State#{options => Options#{host => Host, port => Port},
                      socket => Socket},
      {ok, State2};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, Reason}
  end.

-spec connect_tls(state()) -> {ok, state()} | {error, term()}.
connect_tls(State = #{options := Options}) ->
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, emp:default_port()),
  Timeout = maps:get(connection_timeout, Options, 5000),
  TLSOptions = [{mode, binary}],
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  HostString = unicode:characters_to_list(Host),
  case ssl:connect(HostString, Port, TLSOptions, Timeout) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      State2 = State#{options => Options#{host => Host, port => Port},
                      socket => Socket},
      {ok, State2};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, Reason}
  end.
