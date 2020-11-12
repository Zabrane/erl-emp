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

-module(emp_jsv).

-export([catalog/0,
        format_value_errors/1,
        validate/2]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{op =>
      {object, #{members => #{name => string},
                 required => [name]}},
    ops =>
      {array, #{element => {ref, op}}}}.

-spec format_value_errors([jsv:value_error()]) -> binary().
format_value_errors(Errors) ->
  F = fun (Error) -> ["- ", format_value_error(Error), $\n] end,
  Data = lists:map(F, Errors),
  unicode:characters_to_binary(Data).

-spec format_value_error(jsv:value_error()) -> iodata().
format_value_error(#{pointer := [], reason_string := String}) ->
  ["invalid value: ", String];
format_value_error(#{pointer := Pointer, reason_string := String}) ->
  io_lib:format("invalid value at \"~ts\": ~ts",
                [json_pointer:serialize(Pointer), String]);
format_value_error(#{pointer := []}) ->
  "invalid value";
format_value_error(#{pointer := Pointer}) ->
  io_lib:format("invalid value at \"~ts\"",
                [json_pointer:serialize(Pointer)]).

-spec validate(json:value(), jsv:definition()) ->
        ok | {error, [jsv:value_error()]}.
validate(Value, Definition) ->
  jsv:validate(Value, Definition, #{format_value_errors => true}).
