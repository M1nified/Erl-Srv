-module(example_job).
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

init(Args) ->
  ?DBGF("exmaple_job:init ~p~n",[Args]),
  MArgs = maps:from_list(Args),
  case maps:get(socket,MArgs,empty) of
    empty -> ok;
    Socket ->
      ?DBG("Socket found\n"),
      Ref = make_ref(),
      case gen_tcp:send(Socket,packet:bin_encode({Ref,are_you_there})) of % kinda handshake ;)
      ok ->
        ?DBG("are_you_there -> ok\n");
      {error, Reason} ->
        ?DBGF("failed to send are_you_there: ~p\n",[Reason]),
        error
      end
    end,
  ?DBG("After handshake ~n"),
  {ok,ok}. %initial state

handle_call({Ref,yes_i_am},From,State) ->
  ?DBGF("handle_call({~p,yes_i_am},~p,~p).~n",[Ref,From,State]),
  {reply, end_handshake, State};
handle_call(Request,From,State) ->
  ?DBGF("handle_call(~p,~p,~p).~n",[Request,From,State]),
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


