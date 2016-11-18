-module(test_server_suite).
-export([

]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

get_listensocket__1_test() ->
  {ok,LSock} = test_server:get_lsock(),
  gen_tcp:close(LSock).
get_listensocket__2_test() ->
  {ok,LSock} = test_server:get_lsock(),
  {error,_} = test_server:get_lsock(),
  gen_tcp:close(LSock).

get_socket__1_test() ->
  spawn(fun() -> timer:sleep(3),test_server:connect_worker() end),
  {ok, Socket, ListenSock} = test_server:get_socket(),
  gen_tcp:close(Socket),
  gen_tcp:close(ListenSock).