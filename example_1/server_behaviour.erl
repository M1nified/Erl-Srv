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
-include("../headers/settings.hrl").

-record(state,{
  worker :: worker(),
  iteration = 0 :: integer()
}).
-type state() :: #state{}.

init(Args) ->
  ?DBGF("exmaple_worker:init ~p~n",[Args]),
  MArgs = maps:from_list(Args),
  State = #state{},
  case maps:get(worker,MArgs,empty) of
    empty -> {ok, #state{}};
    Worker ->
      ?DBG("Worker applied\n"),
      {ok, State#state{worker=Worker}}
  end.

handle_call(next_job,_From,State) ->
  % ?DBGF("handle_call State == ~p\n",[State]),
  [{job,Job},{state,NewState}] = next_job(State),
  {reply,Job,NewState};
handle_call(Request,From,State) ->
  ?DBGF("handle_call(~p,~p,~p).~n",[Request,From,State]),
  Response = handle_call_response,
  NewState = State,
  {reply,Response,NewState}.

handle_cast({inbox,Worker,Data},State) ->
  receive_from_inbox(Worker,Data),
  {noreply,State};
handle_cast({result,Result},State) ->
  ?DBGF("handle_cast, received result: ~p",[Result]),
  {noreply,State}.

next_job(State) ->
  [{job,{power,State#state.iteration}},{state,State#state{iteration = State#state.iteration + 1}}].

% old
-spec receive_from_inbox(worker(),tuple()) -> any().
receive_from_inbox(Worker,{Ref,are_you_there}) ->
  OutRef = make_ref(),
  Worker#worker.outbox#thread.pid ! {Worker#worker.head#thread.ref,OutRef,send,{call,{Ref,yes_i_am}}},
  timer:sleep(3000),
  ?DBG("After sleep\n"),
  Worker#worker.outbox#thread.pid ! {Worker#worker.head#thread.ref,OutRef,send,{call,{Ref,next_request}}},
  ok;
receive_from_inbox(Worker, {cluster, Cluster}) ->
  Ref = make_ref(),
  linknode ! {self(),Ref,forward,{self(),Ref,cluster, Cluster}, jobs},
  ok;
receive_from_inbox(Worker,Data) ->
  L = packet:bin_decode(Data),
  ?DBGF("~p\n",[L]),
  ok.

% Unused
terminate(_,_) ->
  ok.
handle_info(_,_) ->
  ok.
code_change(_,_,_) ->
  ok.