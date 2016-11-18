-module(worker).
-export([
  spawn/2
]).
-include("../headers/settings.hrl").

-spec spawn(socket(),jobs_manager_settings()) -> thread().
spawn(Socket,JobsManagerSettings) ->
  WorkerBoss = #thread{pid = self(),ref = make_ref()},
  InboxThread = inbox_spawn(WorkerBoss,Socket,JobsManagerSettings),
  ChiefThread = chief_spawn(WorkerBoss,Socket,JobsManagerSettings),
  TheWorkerRef = make_ref(),
  TheWorkerPid = spawn(fun() -> run(TheWorkerRef,JobsManagerSettings,InboxThread,ChiefThread) end),
  #thread{
    pid = TheWorkerPid,
    ref = TheWorkerRef
  }.

-spec run(reference(),jobs_manager_settings(),thread(),thread()) -> any().
run(TheWorkerRef,JobsManagerSettings,InboxThread,ChiefThread) ->
  ok.

-spec inbox_spawn(thread(),socket(),jobs_manager_settings()) -> thread().
inbox_spawn(Parent,Socket,JobsManagerSettings) ->
  Ref = make_ref(), 
  PID = spawn(fun() -> inbox({Socket,Parent,Ref}) end),
  #thread{pid = PID, ref = Ref}.

-spec inbox({socket(),thread(),reference()}) -> any().
inbox({Socket,Parent,InboxRef}) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Parent#thread.pid ! {#thread{pid = self(),ref = InboxRef},Data};
    {error, Reason} ->
      Parent#thread.pid ! {#thread{pid = self(),ref = InboxRef},error,Reason}
  end.

-spec chief_spawn(thread(),socket(),jobs_manager_settings()) -> thread().
chief_spawn(Parent,Socket,Settings) ->
  Ref = make_ref(),
  PID = spawn(fun() -> chief({Socket,Parent,Ref}) end),
  #thread{pid = PID, ref = Ref}.
  
-spec chief({socket(),thread(),reference()}) -> any().
chief({Socket,Parent,ChiefRef}) ->
  PRef = Parent#thread.ref,
  receive
    {PRef, send, Packet} ->
      case gen_tcp:send(Socket,Packet) of
        ok ->
          Parent#thread.pid ! {#thread{pid = self(),ref = ChiefRef}, send_complete};
        {error, Reason} ->
          Parent#thread.pid ! {#thread{pid = self(),ref = ChiefRef}, error, Reason}
      end,
      chief({Socket,Parent,ChiefRef})
  end.
