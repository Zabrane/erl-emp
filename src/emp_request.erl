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

-export([parse/2, definition/0]).

-export_type([parse_error_reason/0]).

-type parse_error_reason() :: {invalid_data, json:error()}
                            | {invalid_value, [jsv:value_error()]}
                            | {invalid_op, binary()}.

-spec parse(emp_proto:message(), emp:op_table()) ->
        {ok, emp:request()} | {error, parse_error_reason()}.
parse(#{body := #{id := Id, data := Data}}, Ops) ->
  case json:parse(Data) of
    {ok, Value} ->
      case validate(Value, definition()) of
        ok ->
          #{<<"op">> := OpString,
            <<"data">> := DataObject} = Value,
          case validate_data(OpString, DataObject, Ops) of
            ok ->
              Request = #{id => Id,
                          op => binary_to_atom(OpString),
                          data => emp_json:intern_object_keys(DataObject)},
              {ok, Request};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Errors} ->
          {error, {invalid_value, Errors}}
      end;
    {error, Error} ->
      {error, {invalid_data, Error}}
  end.

-spec validate_data(OpString :: binary(), json:value(), emp:op_table()) ->
        ok | {error, parse_error_reason()}.
validate_data(OpString, Value, Ops) ->
  case emp_ops:find_op(OpString, Ops) of
    {ok, #{input := InputDefinition}} ->
      case validate(Value, InputDefinition) of
        ok ->
          ok;
        {error, Errors} ->
          {error, {invalid_value, Errors}}
      end;
    error ->
      {error, {invalid_op, OpString}}
  end.

-spec definition() -> jsv:definition().
definition() ->
  {object, #{members => #{op => string,
                          data => object},
             required => [op, data]}}.

-spec validate(json:value(), jsv:definition()) ->
        ok | {error, [jsv:value_error()]}.
validate(Value, Definition) ->
  jsv:validate(Value, Definition, #{format_value_errors => true}).
