% server worker behaviour
-module(server_behaviour).
-behaviour(gen_server).
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3
]).

-record(state,{
  worker,
  iteration = 0 :: integer()
}).
-type state() :: #state{}.

init(Args) ->
  io:fwrite("exmaple_worker:init ~p~n",[Args]),
  MArgs = maps:from_list(Args),
  State = #state{},
  case maps:get(worker,MArgs,empty) of
    empty -> {ok, #state{}};
    Worker ->
      io:fwrite("Worker applied\n"),
      {ok, State#state{worker=Worker}}
  end.

handle_call(next_job,_From,State) ->
  % io:fwrite("handle_call State == ~p\n",[State]),
  [{job,Job},{state,NewState}] = next_job(State),
  {reply,Job,NewState};
handle_call(Request,From,State) ->
  io:fwrite("handle_call(~p,~p,~p).~n",[Request,From,State]),
  Response = handle_call_response,
  NewState = State,
  {reply,Response,NewState}.

handle_cast({result,{Number,Power}},State) ->
  io:fwrite("Result for ~p is ~p\n",[Number, Power]),
  {noreply,State}.

next_job(State) ->
  [{job,{power,State#state.iteration}},{state,State#state{iteration = State#state.iteration + 1}}].

% Unused
terminate(_,_) ->
  ok.
handle_info(_,_) ->
  ok.
code_change(_,_,_) ->
  ok.