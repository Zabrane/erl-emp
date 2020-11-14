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

-module(emp_test_handler).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([op_table/0, start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{}.

-spec op_table() -> emp:op_table().
op_table() ->
  #{<<"hello">> =>
      #{input =>
          {object, #{members =>
                       #{name => {string, #{min_length => 2}}},
                     required =>
                       [name]}},
        output =>
          {object, #{members =>
                       #{message => string},
                    required =>
                       [message]}}}}.

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) -> {ok, state()}.
init([]) ->
  logger:update_process_metadata(#{domain => [emp, handler, test]}),
  State = #{},
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call({emp_request, Request}, _From, State) ->
  ?LOG_DEBUG("emp request received: ~p", [Request]),
  {reply, handle_request(Request), State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec handle_request(emp:request()) -> emp:response().

handle_request(#{op := <<"hello">>, data := Data}) ->
  Name = maps:get(<<"name">>, Data),
  Message = <<"Hello ", Name/binary, "!">>,
  emp:success_response(#{<<"message">> => Message});

handle_request(#{op := OpName}) ->
  emp:unhandled_op_failure_response(OpName).
