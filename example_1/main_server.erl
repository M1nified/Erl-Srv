-module(main_server).
-export([
  main/1
]).

main(Name) ->
  tcp_cc:start_server(Name, server_behaviour).