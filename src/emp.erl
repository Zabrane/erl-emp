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

-module(emp).

-export([default_port/0,
         send_message/2, send_request/2]).

-export_type([gen_server_name/0, gen_server_ref/0, gen_server_call_tag/0,
              client_id/0, server_id/0]).

-type gen_server_name() :: {local, term()}
                         | {global, term()}
                         | {via, atom(), term()}.

-type gen_server_ref() :: term()
                        | {term(), atom()}
                        | {global, term()}
                        | {via, atom(), term()}
                        | pid().

-type gen_server_call_tag() :: {pid(), term()}.

-type client_id() :: atom().
-type server_id() :: atom().

-type sender() :: {client, client_id()}
                | {connection, pid()}.

-spec default_port() -> inet:port_number().
default_port() ->
  5040.

-spec send_message(sender(), emp_proto:message()) -> ok | {error, term()}.
send_message({client, ClientId}, Message) ->
  ClientRef = emp_client:process_name(ClientId),
  emp_client:send_message(ClientRef, Message);
send_message({connection, Pid}, Message) ->
  emp_connection:send_message(Pid, Message).

-spec send_request(sender(), iodata()) -> {ok, iodata()} | {error, term()}.
send_request({client, ClientId}, Data) ->
  ClientRef = emp_client:process_name(ClientId),
  emp_client:send_request(ClientRef, Data);
send_request({connection, Pid}, Data) ->
  emp_connection:send_request(Pid, Data).
