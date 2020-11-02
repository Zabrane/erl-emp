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

-export([start_link/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0]).

-type options() :: #{}.

%% -type state() :: #{options := options(),
%%                    socket => emp_socket:socket(),
%%                    address := inet:ip_address(),
%%                    port := inet:port_number()}.

-spec start_link(Address, Port, options()) -> Result when
    Address :: inet:ip_address(),
    Port :: inet:port_number(),
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Address, Port, Options) ->
  gen_server:start_link(?MODULE, [Address, Port, Options], []).

init([Address, Port, Options]) ->
  logger:update_process_metadata(#{domain => [emp, connection]}),
  State = #{options => Options,
            address => Address,
            port => Port},
  {ok, State}.

terminate(_Reason, #{socket := Socket}) ->
  emp_socket:close(Socket),
  ok;
terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({socket, Socket}, State) ->
  State2 = State#{socket => Socket},
  emp_socket:setopts(Socket, [{active, 1}]),
  {noreply, State2};

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({Event, _}, _State) when
    Event =:= tcp_closed; Event =:= ssl_closed ->
  ?LOG_INFO("connection closed"),
  exit(normal);

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.
