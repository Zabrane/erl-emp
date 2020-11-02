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

-module(emp_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = server_child_specs(),
  Flags = #{strategy => one_for_one,
            intensity => 1,
            period => 5},
  {ok, {Flags, Children}}.

-spec server_child_specs() -> [supervisor:child_spec()].
server_child_specs() ->
  ServerSpecs = application:get_env(emp, servers, #{}),
  maps:fold(fun (Id, Options, Acc) ->
                [server_child_spec(Id, Options) | Acc]
            end,
            [], ServerSpecs).

-spec server_child_spec(emp:server_id(), emp_server:options()) -> supervisor:child_spec().
server_child_spec(ChildId, Options) ->
  Name = emp_server:process_name(ChildId),
  #{id => ChildId,
    start => {emp_server, start_link, [{local, Name}, Options]}}.
