-module(tcp_cc).
-export([
  start_server/2,
  start_server/3,
  start_terminal/2,
  start_terminal/3
]).

start_server(Name,Module) ->
  start_server(Name, Module,[]).

start_server(_Name,Module,Settings) ->
  spawn(fun() -> server:start(Module,Settings) end).

start_terminal(Name,Module) ->
  start_terminal(Name,Module,[]).
  
start_terminal(_Name,Module,Settings) ->
  spawn(fun() -> terminal:start(Module,Settings) end).