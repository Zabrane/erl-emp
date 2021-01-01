%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(emp_request).

-export([validate/2]).

-export_type([validate_error_reason/0]).

-type validate_error_reason() :: {invalid_value, [jsv:value_error()]}
                               | {unknown_op, binary()}.

-spec validate(emp_proto:message(), emp:op_catalog_name()) ->
        {ok, emp:request()} | {error, validate_error_reason()}.
validate(#{type := request, body := Request}, OpCatalogName) ->
  #{op := OpName, data := Data} = Request,
  case validate_data(OpName, Data, OpCatalogName) of
    ok ->
      {ok, Request};
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_data(emp:op_name(), json:value(), emp:op_catalog_name()) ->
        ok | {error, validate_error_reason()}.
validate_data(OpName, Value, OpCatalogName) ->
  case emp_ops:find_op(OpName, OpCatalogName) of
    {ok, #{input := InputDefinition}} ->
      case emp_jsv:validate(Value, InputDefinition) of
        ok ->
          ok;
        {error, Errors} ->
          {error, {invalid_value, Errors}}
      end;
    error ->
      {error, {unknown_op, OpName}}
  end.
