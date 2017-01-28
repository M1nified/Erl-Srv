-module(main_terminal).
-export([
  main/1
]).

main(Name) ->
  tcp_cc:start_terminal(Name, terminal_behaviour,[
    {terminal_port, 1234},
    {server_address,"localhost"}
  ]).