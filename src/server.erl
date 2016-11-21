-module(server).
-export([
  run/0
]).
-include("../headers/settings.hrl").

run() ->
  ?DBG("run/0\n"),
  case collector:spawn() of
    {ok, ReStoThread} ->
      run(ReStoThread);
    {error, Reason} ->
      {error, run_0, Reason}
  end.

run(ReStoThread) ->
  ?DBG("run/1\n"),
  case display_connector:spawn(ReStoThread) of
    {ok, DiCoThread} ->
      run(ReStoThread,DiCoThread);
    {error, Reason} ->
      {error, run_1, Reason}
  end.

run(ReStoThread,DiCoThread) ->
  ?DBG("run/2\n"),
  case jobs_manager:spawn() of
    {ok, JoMaThread} ->
      run(ReStoThread,DiCoThread,JoMaThread);
    {error, Reason} ->
      {error, run_2, Reason}
  end.

run(ReStoThread,DiCoThread,JoMaThread) ->
  ?DBG("run/3\n"),
  JoMaThread#thread.pid ! {ok, ReStoThread},
  LinkNode = link_node:spawn(),
  spawn(fun() -> server_connector(LinkNode,ReStoThread,DiCoThread,JoMaThread) end).

server_connector(LinkNode,ReStoThread,DiCoThread,JoMaThread) ->
  ?DBG("server_connector\n"),
  receive
    {kill} ->
      ?DBG("GOING FOR THE KILL...");
    {get,buffer} ->
      ?DBG("GETTING BUFFER..."),
      server_connector(LinkNode,ReStoThread,DiCoThread,JoMaThread)
  end.
