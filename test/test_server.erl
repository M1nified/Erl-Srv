-module(test_server).
-export([
  get_lsock/0,
  get_socket/0,
  connect_worker/0
]).
-include("../headers/settings.hrl").

get_lsock() ->
  case gen_tcp:listen(
      ?WORKER_PORT,
      [
        binary,
        {packet, 0},
        {active, false},
        {keepalive, true}
      ]
    ) of
    {ok, ListenSock} -> {ok,ListenSock};
    {error, Reason} -> {error,Reason}
  end.

get_socket() ->
  case gen_tcp:listen(
      ?WORKER_PORT,
      [
        binary,
        {packet, 0},
        {active, false},
        {keepalive, true}
      ]
    ) of
    {ok, ListenSock} ->
      {ok, Sock} = gen_tcp:accept(ListenSock),
      {ok, Sock, ListenSock};
    {error, Reason} -> {error,Reason}
  end.

connect_worker() ->
  gen_tcp:connect(
    "localhost",
    ?WORKER_PORT,
    [
      binary,
      {packet, 0}
    ]
  ).