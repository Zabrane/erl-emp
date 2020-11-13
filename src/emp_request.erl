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

-module(emp_request).

-export([validate/2]).

-export_type([validate_error_reason/0]).

-type validate_error_reason() :: {invalid_value, [jsv:value_error()]}
                               | {invalid_op, binary()}.

-spec validate(emp_proto:message(), emp_ops:op_table_name()) ->
        {ok, emp:request()} | {error, validate_error_reason()}.
validate(#{type := request, body := Request}, OpTableName) ->
  #{op := OpName, data := Data} = Request,
  case validate_data(OpName, Data, OpTableName) of
    ok ->
      Data2 = emp_json:intern_object_keys(Data),
      {ok, Request#{data => Data2}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_data(emp:op_name(), json:value(), emp_ops:op_table_name()) ->
        ok | {error, validate_error_reason()}.
validate_data(OpName, Value, OpTableName) ->
  case emp_ops:find_op(OpName, OpTableName) of
    {ok, #{input := InputDefinition}} ->
      case emp_jsv:validate(Value, InputDefinition) of
        ok ->
          ok;
        {error, Errors} ->
          {error, {invalid_value, Errors}}
      end;
    error ->
      {error, {invalid_op, OpName}}
  end.
