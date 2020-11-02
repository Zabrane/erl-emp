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

-module(emp_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([server_name/0, server_ref/0, options/0]).

-type server_name() :: emp:gen_server_name().
-type server_ref() :: emp:gen_server_ref().

-type options() :: #{address => inet:socket_address(),
                     port => inet:port_number(),
                     transport => emp:transport(),
                     tcp_options => [gen_tcp:connect_option()],
                     tls_options => [ssl:tls_client_option()]}.

-type state() :: #{options := options(),
                   transport := emp:transport(),
                   socket => inet:socket() | ssl:sslsocket()}.

-spec process_name(emp:client_id()) -> atom().
process_name(Id) ->
  Name = <<"emp_server_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(server_name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

init([Options]) ->
  logger:update_process_metadata(#{domain => [emp, server]}),
  case listen(Options) of
    {ok, State} ->
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

terminate(Reason, State = #{transport := tcp, socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, State);
terminate(Reason, State = #{transport := tls, socket := Socket}) ->
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

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec listen(options()) -> {ok, state()} | {error, term()}.
listen(Options) ->
  case maps:get(transport, Options, tcp) of
    tcp ->
      listen_tcp(Options);
    tls ->
      listen_tls(Options)
  end.

-spec listen_tcp(options()) -> {ok, state()} | {error, term()}.
listen_tcp(Options) ->
  Address = maps:get(address, Options, loopback),
  Port = maps:get(port, Options, emp:default_port()),
  RequiredTCPOptions = [{ip, Address},
                        {reuseaddr, true},
                        {active, false},
                        {send_timeout, 5000},
                        {send_timeout_close, true},
                        binary,
                        {packet, 4}],
  TCPOptions = RequiredTCPOptions ++ maps:get(tcp_options, Options, []),
  case gen_tcp:listen(Port, TCPOptions) of
    {ok, Socket} ->
      {ok, {LocalAddress, LocalPort}} = inet:sockname(Socket),
      ?LOG_INFO("listening on ~s:~b", [inet:ntoa(LocalAddress), LocalPort]),
      State = #{options => Options#{address => Address, port => Port},
                transport => tcp,
                socket => Socket},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot listen for connections: ~p", [Reason]),
      {error, Reason}
  end.

-spec listen_tls(options()) -> {ok, state()} | {error, term()}.
listen_tls(Options) ->
  Address = maps:get(address, Options, loopback),
  Port = maps:get(port, Options, emp:default_port()),
  RequiredTLSOptions = [{ip, Address},
                        {reuseaddr, true},
                        {active, false},
                        {send_timeout, 5000},
                        {send_timeout_close, true},
                        binary,
                        {packet, 4}],
  TLSOptions = RequiredTLSOptions ++
    maps:get(tcp_options, Options, []) ++
    maps:get(tls_options, Options, []),
  case ssl:listen(Port, TLSOptions) of
    {ok, Socket} ->
      {ok, {LocalAddress, LocalPort}} = ssl:sockname(Socket),
      ?LOG_INFO("listening on ~s:~b", [inet:ntoa(LocalAddress), LocalPort]),
      State = #{options => Options#{address => Address, port => Port},
                transport => tls,
                socket => Socket},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot listen for connections: ~p", [Reason]),
      {error, {listen, Reason}}
  end.
