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

-export([serialize/1, parse/3, definition/0]).

-export_type([parse_error_reason/0]).

-type parse_error_reason() :: {invalid_data, json:error()}
                            | {invalid_value, [jsv:value_error()]}.

-spec serialize(emp:response()) -> iodata().
serialize(Response) ->
  F = fun
        (status, V, Acc) ->
          Acc#{status => atom_to_binary(V)};
        (error_code, V, Acc) when is_atom(V) ->
          Acc#{error_code => atom_to_binary(V)};
        (error_code, V, Acc) when is_binary(V) ->
          Acc#{error_code => V};
        (description, V, Acc) ->
          Acc#{description => V};
        (data, V, Acc) ->
          Acc#{data => V};
        (_, _, Acc) ->
          Acc
      end,
  Value = maps:fold(F, #{}, Response),
  json:serialize(Value).

-spec parse(emp_proto:message(), emp:op_name(), emp:op_table()) ->
        {ok, emp:response()} | {error, parse_error_reason()}.
parse(#{body := #{data := Data}}, OpName, Ops) ->
  case json:parse(Data) of
    {ok, Value} ->
      case emp_jsv:validate(Value, definition()) of
        ok ->
          #{<<"status">> := StatusString} = Value,
          Status = binary_to_atom(StatusString),
          DataObject = maps:get(<<"data">>, Value, #{}),
          case validate_data(OpName, Status, DataObject, Ops) of
            ok ->
              {ok, parse_value(Value)};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Errors} ->
          {error, {invalid_value, Errors}}
      end;
    {error, Error} ->
      {error, {invalid_data, Error}}
  end.

-spec parse_value(json:value()) -> emp:response().
parse_value(Value) ->
  F = fun
        (<<"status">>, V, Acc) ->
          Acc#{status => binary_to_atom(V)};
        (<<"error_code">>, V, Acc) ->
          Acc#{error_code => binary_to_atom(V)};
        (<<"data">>, V, Acc) ->
          Acc#{data => emp_json:intern_object_keys(V)};
        (<<"description">>, V, Acc) ->
          Acc#{description => V}
      end,
  maps:fold(F, #{}, Value).

-spec validate_data(emp:op_name(), emp:response_status(), json:value(),
                    emp:op_table()) ->
        ok | {error, parse_error_reason()}.
validate_data(OpName, Status, Value, Ops) ->
  {ok, Op} = emp_ops:find_op(OpName, Ops),
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

-spec definition() -> jsv:definition().
definition() ->
  {object, #{members => #{status => {string, #{values => [success, failure]}},
                          error_code => string,
                          description => string,
                          data => object},
             required => [status]}}.
