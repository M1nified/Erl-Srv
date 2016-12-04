-module(worker_terminal).
-export([
  init/0
]).
-include("../headers/settings.hrl").

init() ->
  case gen_tcp:connect(?SERVER_ADDR, ?WORKER_PORT, ?WORKER_TERMINAL_OPTIONS) of
    {ok, Socket} ->
      connected(Socket);
    {error, Reason} ->
      ?DBGF("Connect error: ~p\n",[Reason]),
      init()
  end,
  ok.

connected(Socket) ->
  Ref = make_ref(),
  loop(Socket),
  case gen_tcp:send(Socket,packet:bin_encode({Ref,are_you_there})) of % kinda handshake ;)
    ok ->
      ?DBG("are_you_there -> ok\n"),
      % case gen_tcp:recv(Socket,0) of
      receive
        {tcp,Socket,Data} ->
          ?DBGF("are_you_there -> ~p\n",[packet:bin_decode(Data)]),
          ok;
        {error, Reason} ->
          ?DBGF("failed to receive response for are_you_there: ~p\n",[Reason]),
          error;
        Other ->
          ?DBGF("got other response for are_you_there: ~p\n",[Other]),
          other
      end;
    {error, Reason} ->
      ?DBGF("failed to send are_you_there: ~p\n",[Reason]),
      error
  end.

loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      tcp_recv(Socket,packet:bin_decode(Data));
    {cluster, Cluster} ->
      gen_tcp:send(Socket,packet:bin_encode(Cluster)),
      loop(Socket)
  end.

tcp_recv(Socket,{WorkerReference,new_cluster,Source,Time_0,FramesToCount}) when is_integer(FramesToCount)->
  Cluster = animation:spawn_cluster(Source,Time_0),
  TerminalPID = self(),
  spawn(fun() -> animate_cluster(TerminalPID,Cluster,FramesToCount) end),
  loop(Socket);

tcp_recv(Socket,{WorkerReference,add_cluster, Cluster, FramesToCount}) when is_integer(FramesToCount) ->
  TerminalPID = self(),
  spawn(fun() -> animate_cluster(TerminalPID,Cluster,FramesToCount) end),
  loop(Socket);

tcp_recv(_,_) ->
  trash.

animate_cluster(_,_,0) -> ok;
animate_cluster(TerminalPID,Cluster, FramesToCount) when is_integer(FramesToCount)->
  NewCluster = physics:step_cluster(Cluster,Cluster#cluster.time+?TIME_STEP),
  TerminalPID ! {cluster, NewCluster},
  animate_cluster(TerminalPID,NewCluster,FramesToCount-1).