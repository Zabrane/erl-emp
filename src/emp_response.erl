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

-module(emp_response).

-export([validate/3]).

-export_type([validate_error_reason/0]).

-type validate_error_reason() :: {invalid_value, [jsv:value_error()]}.

-spec validate(emp_proto:message(), emp:op_name(), emp:op_catalog_name()) ->
        {ok, emp:response()} | {error, validate_error_reason()}.
validate(#{type := response, body := Response}, OpName, OpCatalogName) ->
  #{status := Status, data := Data} = Response,
  case validate_data(OpName, Status, Data, OpCatalogName) of
    ok ->
      {ok, Response};
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_data(emp:op_name(), emp:response_status(), json:value(),
                    emp:op_catalog_name()) ->
        ok | {error, validate_error_reason()}.
validate_data(OpName, Status, Value, OpCatalogName) ->
  {ok, Op} = emp_ops:find_op(OpName, OpCatalogName),
  DefType = case Status of
              success -> output;
              failure -> error
            end,
  Definition = maps:get(DefType, Op, object),
  case emp_jsv:validate(Value, Definition) of
    ok ->
      ok;
    {error, Errors} ->
      {error, {invalid_value, Errors}}
  end.
