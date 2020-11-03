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

-module(emp_proto_test).

-include_lib("eunit/include/eunit.hrl").

encode_string_test_() ->
  Encode = fun emp_proto:encode_string/1,
  [?_assertEqual(<<0, 0>>, Encode(<<"">>)),
   ?_assertEqual(<<0, 1, 97>>, Encode(<<"a">>)),
   ?_assertEqual(<<0, 3, 102, 111, 111>>, Encode(<<"foo">>)),
   ?_assertEqual(<<0, 5, 195, 169, 116, 195, 169>>, Encode(<<"été"/utf8>>)),
   ?_assertEqual(<<0, 4, 240, 157, 132, 158>>, Encode(<<"𝄞"/utf8>>))].

decode_string_test_() ->
  Decode = fun emp_proto:decode_string/1,
  [?_assertEqual(<<"">>, Decode(<<0, 0>>)),
   ?_assertEqual(<<"a">>, Decode(<<0, 1, 97>>)),
   ?_assertEqual(<<"foo">>, Decode(<<0, 3, 102, 111, 111>>)),
   ?_assertEqual(<<"été"/utf8>>, Decode(<<0, 5, 195, 169, 116, 195, 169>>)),
   ?_assertEqual(<<"𝄞"/utf8>>, Decode(<<0, 4, 240, 157, 132, 158>>))].

encode_decode_message_test_() ->
  Messages = [emp_proto:hello_message(),
              emp_proto:bye_message(),
              emp_proto:ping_message(),
              emp_proto:pong_message(),
              emp_proto:error_message(unspecified_error, "test"),
              emp_proto:error_message(unspecified_error, <<"test">>),
              emp_proto:error_message(unspecified_error, "a: ~b", [42]),
              emp_proto:data_message(<<>>),
              emp_proto:data_message(<<1, 2, 3>>)],
  [encode_decode_message_(M) || M <- Messages].

encode_decode_message_(Message) ->
  Data = iolist_to_binary(emp_proto:encode_message(Message)),
  ?_assertEqual({ok, Message}, emp_proto:decode_message(Data)).