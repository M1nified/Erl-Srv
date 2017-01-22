-module(main_terminal).
-export([
  main/1
]).

main(Name) ->
  tcp_cc:start_terminal(Name, terminal_behaviour).