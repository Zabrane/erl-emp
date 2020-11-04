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
                     transport => emp_socket:transport(),
                     socket_options => emp_socket:listen_options(),
                     connection_options => emp_connection:options()}.

-type state() :: #{options := options(),
                   socket => emp_socket:socket()}.

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

terminate(_Reason, #{socket := Socket}) ->
  emp_socket:close(Socket),
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
  Transport = maps:get(transport, Options, tcp),
  Address = maps:get(address, Options, loopback),
  Port = maps:get(port, Options, emp:default_port()),
  SocketOptions = [{ip, Address},
                   {reuseaddr, true},
                   {send_timeout, 5000},
                   {send_timeout_close, true},
                   {active, false},
                   binary,
                   {packet, 4}] ++
    maps:get(socket_options, Options, []),
  case emp_socket:listen(Transport, Port, SocketOptions) of
    {ok, Socket} ->
      {ok, {LocalAddress, LocalPort}} = emp_socket:sockname(Socket),
      ?LOG_INFO("listening on ~s:~b", [inet:ntoa(LocalAddress), LocalPort]),
      {ok, _} = emp_acceptor:start_link(Socket, Options),
      State = #{options => Options,
                socket => Socket},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot listen for connections: ~p", [Reason]),
      {error, Reason}
  end.
