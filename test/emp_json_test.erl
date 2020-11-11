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

-module(emp_json_test).

-include_lib("eunit/include/eunit.hrl").

intern_object_keys_test_() ->
  [?_assertEqual(42, emp_json:intern_object_keys(42)),
   ?_assertEqual(3.14, emp_json:intern_object_keys(3.14)),
   ?_assertEqual(true, emp_json:intern_object_keys(true)),
   ?_assertEqual(null, emp_json:intern_object_keys(null)),
   ?_assertEqual(#{}, emp_json:intern_object_keys(#{})),
   ?_assertEqual(#{a => 1, b => <<"c">>},
                 emp_json:intern_object_keys(#{a => 1, <<"b">> => <<"c">>})),
   ?_assertEqual([#{a => 1, b => <<"c">>}],
                 emp_json:intern_object_keys([#{a => 1, <<"b">> => <<"c">>}]))].
