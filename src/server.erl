-module(server).
-export([
  run/0,
  run/1
]).
-include("../headers/server_header.hrl").

run() ->
  run(?WORKER).

run(BehaviourModule) ->
  ?DBG("run/0\n"),
  case collector:spawn() of
    {ok, CollectorThr} ->
      run(BehaviourModule,CollectorThr);
    {error, Reason} ->
      {error, run_0, Reason}
  end.

run(BehaviourModule,CollectorThr) ->
  ?DBG("run/1\n"),
  case display_connector:spawn() of
    {ok, DiCoThread} ->
      run(BehaviourModule,CollectorThr,DiCoThread);
    {error, Reason} ->
      {error, run_1, Reason}
  end.

run(BehaviourModule,CollectorThr,DiCoThread) ->
  ?DBG("run/2\n"),
  case jobs_manager:spawn() of
    {ok, JobsThread} ->
      run(BehaviourModule,CollectorThr,DiCoThread,JobsThread);
    {error, Reason} ->
      {error, run_2, Reason}
  end.

run(BehaviourModule,CollectorThr,DiCoThread,JobsThread) ->
  ?DBG("run/3\n"),
  LinkNode = link_node:spawn(),
  register(linknode,LinkNode),
  RegCollectorThrRef = make_ref(),
  RegDisplayThrRef = make_ref(),
  RegJobsThrRef = make_ref(),
  linknode ! {self(),RegCollectorThrRef,reg,collector,CollectorThr#thread.pid},
  linknode ! {self(),RegDisplayThrRef,reg,display,DiCoThread#thread.pid},
  linknode ! {self(),RegJobsThrRef,reg,jobs,JobsThread#thread.pid},
  spawn(fun() -> server_connector(LinkNode) end),
  JobsThread#thread.pid ! {behaviour_module, BehaviourModule},
  linknode ! {self(),make_ref(),forward,ready,to_all}.

server_connector(LinkNode) ->
  ?DBG("server_connector\n"),
  receive
    {kill} ->
      ?DBG("GOING FOR THE KILL...");
    {get,buffer} ->
      ?DBG("GETTING BUFFER..."),
      server_connector(LinkNode)
  end.
