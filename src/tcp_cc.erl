-module(tcp_cc).
-export([
  start_server/2,
  start_terminal/2
]).

start_server(_Name,Module) ->
  spawn(fun() -> server:run(Module) end).

start_terminal(_Name,Module) ->
  spawn(fun() -> terminal:init(Module) end).