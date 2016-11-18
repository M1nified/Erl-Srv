-module(server).
-export([
  run/0
]).
-include("../headers/settings.hrl").

run() ->
  case ready_storage:spawn() of
    {ok, ReStoThread} ->
      run(ReStoThread);
    {error, Reason} ->
      {error, run_0, Reason}
  end.

run(ReStoThread) ->
  case display_connector:spawn() of
    {ok, DiCoThread} ->
      run(ReStoThread,DiCoThread);
    {error, Reason} ->
      {error, run_1, Reason}
  end.

run(ReStoThread,DiCoThread) ->
  case jobs_manager:spawn() of
    {ok, JoMaThread} ->
      run(ReStoThread,DiCoThread,JoMaThread);
    {error, Reason} ->
      {error, run_2, Reason}
  end.

run(ReStoThread,DiCoThread,JoMaThread) ->
  JoMaThread#thread.pid ! {ok, ReStoThread},
  spawn(fun() -> server_connector(ReStoThread,DiCoThread,JoMaThread) end).

server_connector(ReStoThread,DiCoThread,JoMaThread) ->
  receive
    {kill} ->
      ?DBG("GOING FOR THE KILL...");
    {get,buffer} ->
      ?DBG("GETTING BUFFER..."),
      server_connector(ReStoThread,DiCoThread,JoMaThread)
  end.