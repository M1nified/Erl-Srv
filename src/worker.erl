-module(worker).
-export([
  spawn/2
]).
-include("../headers/settings.hrl").

-spec spawn(socket(),jobs_manager_settings()) -> thread().
spawn(Socket,JobsManagerSettings) ->
  % WorkerBoss = #thread{pid = self(),ref = make_ref()},
  InboxThread = inbox_spawn(),
  OutboxThread = outbox_spawn(),
  TheWorkerRef = make_ref(),
  TheWorkerPid = spawn(fun() -> run(TheWorkerRef,JobsManagerSettings,InboxThread,OutboxThread, Socket) end),



  #thread{
    pid = TheWorkerPid,
    ref = TheWorkerRef
  }.

-spec run(reference(),jobs_manager_settings(),thread(),thread(),socket()) -> any().
run(TheWorkerRef,JobsManagerSettings,InboxThread,OutboxThread, Socket) ->
  ?DBGF("~p Worker spawned and running...\n", [TheWorkerRef]),
  Worker = #worker{
    head = #thread{pid = self(), ref = TheWorkerRef},
    inbox = InboxThread,
    outbox = OutboxThread,
    socket = Socket
  },
  Worker#worker.inbox#thread.pid ! {worker, Worker},
  Worker#worker.outbox#thread.pid ! {worker, Worker},
  worker_loop(Worker),
  ok.

-spec worker_loop(worker()) -> any().
worker_loop(Worker) ->
  InboxRef = Worker#worker.inbox#thread.ref,
  receive
    {inbox,InboxRef, {error, Reason}} ->
      ?DBGF("~p Received error from inbox: ~p\n",[Worker#worker.head#thread.ref, Reason]),
      worker_loop(Worker);
    {inbox,InboxRef, Data} ->
      receive_from_inbox(Worker,Data),
      worker_loop(Worker)
  end.

-spec receive_from_inbox(worker(),any()) -> any().
receive_from_inbox(Worker,Data) ->
  ?DBGF("~p Received from worker's inbox: ~p\n",[Worker#worker.head#thread.ref,Data]),
  ok.

-spec inbox_spawn() -> thread().
inbox_spawn() ->
  Ref = make_ref(), 
  PID = spawn(fun() -> inbox() end),
  #thread{pid = PID, ref = Ref}.

-spec inbox() -> any().
inbox() ->
  receive
    {worker, Worker} ->
      ?DBGF("~p Worker inbox is going to loop\n",[Worker#worker.head#thread.ref]),
      inbox_loop(Worker)
  end.

-spec inbox_loop(worker()) -> any().
inbox_loop(Worker) ->
  ?DBGF("~p inbox_loop\n",[Worker#worker.head#thread.ref]),
  case gen_tcp:recv(Worker#worker.socket, 0) of
    {ok, Data} ->
      Worker#worker.head#thread.pid ! {inbox,Worker#worker.inbox#thread.ref,Data},
      inbox_loop(Worker);
    {error, Reason} ->
      Worker#worker.head#thread.pid ! {inbox,Worker#worker.inbox#thread.ref,{error, Reason}},
      inbox_loop(Worker)
  end.

-spec outbox_spawn() -> thread().
outbox_spawn() ->
  Ref = make_ref(),
  PID = spawn(fun() -> outbox() end),
  #thread{pid = PID, ref = Ref}.
  
-spec outbox() -> any().
outbox() ->
  receive
    {worker, Worker} ->
      ?DBGF("~p Worker outbox is going to loop\n",[Worker#worker.head#thread.ref]),
      outbox_loop(Worker)
  end.

-spec outbox_loop(worker()) -> any().
outbox_loop(Worker) ->
  HeadRef = Worker#worker.head#thread.ref,
  receive
    {HeadRef, MsgRef, send, Packet} ->
      case gen_tcp:send(Worker#worker.socket,Packet) of
        ok ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref,MsgRef,ok};
        {error, Reason} ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref, MsgRef,{error, Reason}}
      end,
      outbox_loop(Worker)
  end.
