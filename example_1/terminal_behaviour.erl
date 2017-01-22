% terminal behaviour
-module(terminal_behaviour).
-behaviour(gen_server).
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3
]).

init(Args) ->
  io:fwrite("exmaple_job:init ~p~n",[Args]),
  MArgs = maps:from_list(Args),
  case maps:get(socket,MArgs,empty) of
    empty -> ok;
    _Socket ->
      io:fwrite("Socket found\n")
  end,
  io:fwrite("After handshake ~n"),
  {ok,ok}. %initial state

handle_call({power,Number},_From,State) ->
  io:fwrite("power: ~p~n",[Number]),
  Response = {Number, math:pow(Number,2)},
  % Response = {Number, math:pow(Number,Number)},
  timer:sleep(500),
  {reply, Response, State};
handle_call(Request,From,State) ->
  io:fwrite("handle_call(~p,~p,~p).~n",[Request,From,State]),
  Response = handle_call_response,
  NewState = State,
  {reply,Response,NewState}.

% Unused
terminate(_,_) ->
  ok.
handle_cast(_,_) ->
  ok.
handle_info(_,_) ->
  ok.
code_change(_,_,_) ->
  ok.


