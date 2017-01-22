-module(main_server).
-export([
  main/1
]).

main(Name) ->
  packet:bin_encode(term).
  % tcp_cc:start_server(Name, server_behaviour).