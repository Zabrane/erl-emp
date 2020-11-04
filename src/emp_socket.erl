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

-module(emp_socket).

-export([connect/5, listen/3, accept/2,
         sockname/1, peername/1, setopts/2,
         controlling_process/2, send/2, close/1]).

-export_type([transport/0, socket/0,
              connect_options/0, listen_options/0]).

-type transport() :: tcp | tls.

-type socket() :: {tcp, inet:socket()}
                | {tls, ssl:sslsocket()}.

-type connect_options() ::
        [gen_tcp:connect_option()] | [ssl:tls_client_option()].

-type listen_options() ::
        [gen_tcp:listen_option()] | [ssl:tls_server_option()].

-type socket_options() ::
        [inet:socket_setopt()] | [gen_tcp:option()].

-spec connect(transport(), inet:socket_address() | inet:hostname(),
              inet:port_number(), connect_options(), timeout()) ->
        {ok, socket()} | {error, term()}.
connect(tcp, Hostname, Port, Options, Timeout) ->
  case gen_tcp:connect(Hostname, Port, Options, Timeout) of
    {ok, Socket} ->
      {ok, {tcp, Socket}};
    {error, Reason} ->
      {error, Reason}
  end;
connect(tls, Hostname, Port, Options, Timeout) ->
  case ssl:connect(Hostname, Port, Options, Timeout) of
    {ok, Socket} ->
      {ok, {tls, Socket}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec listen(transport(), inet:port_number(), listen_options()) ->
        {ok, socket()} | {error, term()}.
listen(tcp, Port, Options) ->
  case gen_tcp:listen(Port, Options) of
    {ok, Socket} ->
      {ok, {tcp, Socket}};
    {error, Reason} ->
      {error, Reason}
  end;
listen(tls, Port, Options) ->
  case ssl:listen(Port, Options) of
    {ok, Socket} ->
      {ok, {tls, Socket}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec accept(socket(), timeout()) -> {ok, socket()} | {error, term()}.
accept({tcp, Socket}, Timeout) ->
  case gen_tcp:accept(Socket, Timeout) of
    {ok, ConnSocket} ->
      {ok, {tcp, ConnSocket}};
    {error, Reason} ->
      {error, Reason}
  end;
accept({tls, Socket}, Timeout) ->
  %% TODO Timeout should cover both operations
  case ssl:transport_accept(Socket, Timeout) of
    {ok, ConnSocket1} ->
      case ssl:handshake(ConnSocket1, Timeout) of
        {ok, ConnSocket2} ->
          {ok, {tls, ConnSocket2}};
        {error, Reason} ->
          ssl:close(ConnSocket1),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec sockname(socket()) ->
        {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
sockname({tcp, Socket}) ->
  inet:sockname(Socket);
sockname({tls, Socket}) ->
  ssl:sockname(Socket).

-spec peername(socket()) ->
        {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
peername({tcp, Socket}) ->
  inet:peername(Socket);
peername({tls, Socket}) ->
  ssl:peername(Socket).

-spec setopts(socket(), socket_options()) -> ok | {error, term()}.
setopts({tcp, Socket}, Options) ->
  inet:setopts(Socket, Options);
setopts({tls, Socket}, Options) ->
  ssl:setopts(Socket, Options).

-spec controlling_process(socket(), pid()) -> ok | {error, term()}.
controlling_process({tcp, Socket}, Pid) ->
  gen_tcp:controlling_process(Socket, Pid);
controlling_process({tls, Socket}, Pid) ->
  ssl:controlling_process(Socket, Pid).

-spec send(socket(), iodata()) -> ok | {error, term()}.
send({tcp, Socket}, Data) ->
  gen_tcp:send(Socket, Data);
send({tls, Socket}, Data) ->
  ssl:send(Socket, Data).

-spec close(socket()) -> ok | {error, term()}.
close({tcp, Socket}) ->
  inet:close(Socket);
close({tls, Socket}) ->
  ssl:close(Socket).
