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

-module(emp_acceptor).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{options := emp_server:options(),
                   socket => emp_socket:socket()}.

-spec start_link(emp_socket:socket(), emp_server:options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Socket, Options) ->
  gen_server:start_link(?MODULE, [Socket, Options], []).

init([Socket, Options]) ->
  logger:update_process_metadata(#{domain => [emp, acceptor]}),
  process_flag(trap_exit, true),
  State = #{options => Options,
            socket => Socket},
  gen_server:cast(self(), accept),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(accept, State = #{socket := Socket}) ->
  case emp_socket:accept(Socket, 1000) of
    {ok, ConnSocket} ->
      case emp_socket:peername(ConnSocket) of
        {ok, {ConnAddress, ConnPort}} ->
          ?LOG_DEBUG("connection accepted from ~s:~b",
                     [inet:ntoa(ConnAddress), ConnPort]),
          spawn_connection(ConnSocket, ConnAddress, ConnPort, State),
          gen_server:cast(self(), accept),
          {noreply, State};
        {error, Reason} ->
          ?LOG_ERROR("cannot obtain peer address and port: ~p", Reason),
          emp_socket:close(ConnSocket),
          gen_server:cast(self(), accept),
          {noreply, State}
      end;
    {error, timeout} ->
      gen_server:cast(self(), accept),
      {noreply, State};
    {error, closed} ->
      {noreply, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot accept connection: ~p", [Reason]),
      exit({accept_failure, Reason})
  end;

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_WARNING("connection ~p exited (~p)", [Pid, Reason]),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec spawn_connection(emp_socket:socket(),
                       inet:ip_address(), inet:port_number(),
                       state()) ->
        ok.
spawn_connection(Socket, Address, Port, #{options := Options}) ->
  ConnOptions = maps:get(connection_options, Options, #{}),
  {ok, Pid} = emp_connection:start_link(Address, Port, ConnOptions),
  ok = emp_socket:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {socket, Socket}),
  ok.
