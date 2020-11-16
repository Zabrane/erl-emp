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

-export([find_op/2, all_ops/1, serialize_op/2,
         internal_op_catalog/0]).

-spec find_op(emp:op_name(), emp:op_catalog_name()) ->
        {ok, emp:op()} | error.
find_op(Name, OpCatalogName) ->
  OpTableName = emp_op_catalog_registry:table_name(OpCatalogName),
  case ets:whereis(OpTableName) of
    undefined ->
      error({missing_op_catalog, OpCatalogName, OpTableName});
    Ref ->
      case ets:lookup(Ref, Name) of
        [{_, Op}] -> {ok, Op};
        [] -> error
      end
  end.

-spec all_ops(emp:op_catalog_name()) ->
        #{emp:op_name() => emp:op()}.
all_ops(OpCatalogName) ->
  OpTableName = emp_op_catalog_registry:table_name(OpCatalogName),
  maps:from_list(ets:tab2list(OpTableName)).

-spec serialize_op(emp:op_name(), emp:op()) -> json:value().
serialize_op(OpName, _Op) ->
  %% TODO
  #{name => OpName}.

-spec internal_op_catalog() -> emp:op_catalog().
internal_op_catalog() ->
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
