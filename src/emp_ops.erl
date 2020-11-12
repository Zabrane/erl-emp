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

-module(emp_ops).

-export([default_op_table_name/0, install_op_table/2,
         find_op/2,
         default_ops/0]).

-export_type([op_table_name/0]).

-type op_table_name() :: atom().

-spec default_op_table_name() -> op_table_name().
default_op_table_name() ->
  emp_ops_table.

-spec install_op_table(emp:op_table(), op_table_name()) -> ok.
install_op_table(Ops, TableName) ->
  ets:new(TableName, [set,
                      named_table,
                      {read_concurrency, true}]),
  lists:foreach(fun (Pair) ->
                    ets:insert(TableName, Pair)
                end, maps:to_list(Ops)),
  ok.

-spec find_op(emp:op_name() | binary(), emp_ops:op_table_name()) ->
        {ok, emp:op()} | error.
find_op(Name, OpTableName) when is_atom(Name) ->
  find_op(atom_to_binary(Name), OpTableName);
find_op(String, OpTableName) ->
  case ets:lookup(OpTableName, String) of
    [{_, Op}] -> {ok, Op};
    [] -> error
  end.

-spec default_ops() -> emp:op_table().
default_ops() ->
  #{<<"$echo">> =>
      #{input => object,
        output => object},
   <<"$get_op">> =>
      #{input =>
          {object, #{members =>
                       #{op_name => string},
                     required =>
                       [op_name]}},
        output =>
          {object, #{members =>
                       #{op => {ref, emp, op}},
                     required =>
                       [op]}}},
   <<"$list_ops">> =>
      #{input =>
          object,
        output =>
          {object, #{members =>
                       #{ops => {ref, emp, ops}},
                     required =>
                       [ops]}}}}.
