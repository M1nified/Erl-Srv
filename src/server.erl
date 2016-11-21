-module(server).
-export([
  run/0
]).
-include("../headers/settings.hrl").

run() ->
  ?DBG("run/0\n"),
  case collector:spawn() of
    {ok, CollectorThr} ->
      run(CollectorThr);
    {error, Reason} ->
      {error, run_0, Reason}
  end.

run(CollectorThr) ->
  ?DBG("run/1\n"),
  case display_connector:spawn(CollectorThr) of
    {ok, DiCoThread} ->
      run(CollectorThr,DiCoThread);
    {error, Reason} ->
      {error, run_1, Reason}
  end.

run(CollectorThr,DiCoThread) ->
  ?DBG("run/2\n"),
  case jobs_manager:spawn() of
    {ok, JobsThread} ->
      run(CollectorThr,DiCoThread,JobsThread);
    {error, Reason} ->
      {error, run_2, Reason}
  end.

run(CollectorThr,DiCoThread,JobsThread) ->
  ?DBG("run/3\n"),
  JobsThread#thread.pid ! {ok, CollectorThr},
  LinkNode = link_node:spawn(),
  RegCollectorThrRef = make_ref(),
  RegDisplayThrRef = make_ref(),
  RegJobsThrRef = make_ref(),
  LinkNode ! {self(),RegCollectorThrRef,reg,collector,CollectorThr#thread.pid},
  LinkNode ! {self(),RegDisplayThrRef,reg,display,DiCoThread#thread.pid},
  LinkNode ! {self(),RegJobsThrRef,reg,jobs,JobsThread#thread.pid},
  spawn(fun() -> server_connector(LinkNode) end).

server_connector(LinkNode) ->
  ?DBG("server_connector\n"),
  receive
    {kill} ->
      ?DBG("GOING FOR THE KILL...");
    {get,buffer} ->
      ?DBG("GETTING BUFFER..."),
      server_connector(LinkNode)
  end.
