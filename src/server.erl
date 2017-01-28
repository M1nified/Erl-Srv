-module(server).
-export([
  start/1,
  start/2
]).
-include("../headers/server_header.hrl").

start(BehaviourModule) ->
  start(BehaviourModule,[]).
start(BehaviourModule,ServerSettings) ->
  ?DBG("start/0\n"),
  case collector:spawn() of
    {ok, CollectorThr} ->
      start(BehaviourModule,ServerSettings,CollectorThr);
    {error, Reason} ->
      {error, start_0, Reason}
  end.

start(BehaviourModule,ServerSettings,CollectorThr) ->
  ?DBG("start/1\n"),
  case display_connector:spawn() of
    {ok, DiCoThread} ->
      start(BehaviourModule,ServerSettings,CollectorThr,DiCoThread);
    {error, Reason} ->
      {error, start_1, Reason}
  end.

start(BehaviourModule,ServerSettings,CollectorThr,DiCoThread) ->
  ?DBG("start/2\n"),
  case jobs_manager:spawn() of
    {ok, JobsThread} ->
      start(BehaviourModule,ServerSettings,CollectorThr,DiCoThread,JobsThread);
    {error, Reason} ->
      {error, start_2, Reason}
  end.

start(BehaviourModule,ServerSettings,CollectorThr,DiCoThread,JobsThread) ->
  ?DBG("start/3\n"),
  LinkNode = link_node:spawn(),
  register(linknode,LinkNode),
  RegCollectorThrRef = make_ref(),
  RegDisplayThrRef = make_ref(),
  RegJobsThrRef = make_ref(),
  linknode ! {self(),RegCollectorThrRef,reg,collector,CollectorThr#thread.pid},
  linknode ! {self(),RegDisplayThrRef,reg,display,DiCoThread#thread.pid},
  linknode ! {self(),RegJobsThrRef,reg,jobs,JobsThread#thread.pid},
  spawn(fun() -> server_connector(LinkNode) end),
  JobsThread#thread.pid ! {
    {behaviour_module, BehaviourModule},
    {settings, ServerSettings}
  },
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
