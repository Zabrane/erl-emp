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

-module(emp_tests).

-include_lib("eunit/include/eunit.hrl").

-export([start_test_server/1, start_test_client/1]).

internal_ops_test_() ->
  {spawn,
   {setup,
    fun () ->
        application:ensure_all_started(emp),
        emp_test:start_test_server(#{}),
        emp_test:start_test_client(#{})
    end,
    fun (_) ->
        ok
    end,
    [fun internal_ops_test_echo/0,
     fun internal_ops_test_get_op/0,
     fun internal_ops_test_list_ops/0]}}.

internal_ops_test_echo() ->
  ?assertMatch({ok, #{<<"a">> := 1}},
               send_test_request(<<"$echo">>, #{<<"a">> => 1})).

internal_ops_test_get_op() ->
  ?assertMatch({ok, #{<<"op">> := #{<<"name">> := <<"$echo">>}}},
               send_test_request(<<"$get_op">>, #{op_name => <<"$echo">>})),
  ?assertMatch({error, {request_failure, _, #{}}},
               send_test_request(<<"$get_op">>, #{op_name => <<"foobar">>})).

internal_ops_test_list_ops() ->
  ?assertMatch({ok, #{<<"ops">> := _}},
               send_test_request(<<"$list_ops">>)).

-spec start_test_server(emp_server:options()) -> ok.
start_test_server(Options) ->
  Name = emp_server:process_name(test),
  emp_server:start_link({local, Name}, Options).

-spec start_test_client(emp_client:options()) -> ok.
start_test_client(Options) ->
  Name = emp_client:process_name(test),
  emp_client:start_link({local, Name}, Options).

-spec send_test_request(emp:op_name()) ->
        {ok, emp:response()} | {error, term()}.
send_test_request(OpName) ->
  emp:send_request({client, test}, OpName).

-spec send_test_request(emp:op_name(), json:value()) ->
        {ok, emp:response()} | {error, term()}.
send_test_request(OpName, Data) ->
  emp:send_request({client, test}, OpName, Data).
