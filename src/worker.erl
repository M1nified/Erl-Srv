-module(worker).
-export([
  spawn/2
]).
-include("../headers/server_header.hrl").

-spec spawn(socket(),jobs_manager_settings()) -> thread() | error.
spawn(Socket,JobsManagerSettings) ->
  % WorkerBoss = #thread{pid = self(),ref = make_ref()},
  InboxThread = inbox_spawn(),
  OutboxThread = outbox_spawn(),
  TheWorkerRef = make_ref(),
  TheWorkerPid = spawn(fun() -> run(TheWorkerRef,JobsManagerSettings,InboxThread,OutboxThread, Socket) end),
  Ref = make_ref(),
  TheWorkerPid ! {self(),Ref,getworker},
  receive
    {Ref,{worker,Worker}} ->
      Worker;
    _ ->
      error
  end.

-spec run(reference(),jobs_manager_settings(),thread(),thread(),socket()) -> any().
run(TheWorkerRef,JobsManagerSettings,InboxThread,OutboxThread, Socket) ->
  Worker = #worker{
    head = #thread{pid = self(), ref = TheWorkerRef},
    inbox = InboxThread,
    outbox = OutboxThread,
    socket = Socket,
    jmgr = JobsManagerSettings#jobs_manager_settings.jobsmanager,
    bm = JobsManagerSettings#jobs_manager_settings.bm
  },
  JobsManagerSettings#jobs_manager_settings.jobsmanager#thread.pid ! {self(),TheWorkerRef,register_worker,Worker},
  ?DBGF("~p Worker spawned and running...\n~p\n", [TheWorkerRef,Worker]),
  Worker#worker.inbox#thread.pid ! {worker, Worker},
  Worker#worker.outbox#thread.pid ! {worker, Worker},
  gen_server:start_link({local,Worker#worker.bm},Worker#worker.bm,[{worker,Worker}],[{debug,[log]}]),
  worker_loop(Worker),
  ok.

-spec worker_loop(worker()) -> any().
worker_loop(Worker) ->
  InboxRef = Worker#worker.inbox#thread.ref,
  receive
    {jms, assignment, {MethodType, {task, Task}}} ->
      ?DBGF("~p Received task: ~p\n",[Worker#worker.head#thread.ref, Task]),
      Worker#worker.outbox#thread.pid ! {Worker#worker.head#thread.ref, make_ref(), send, {MethodType, {task, Task}}},
      worker_loop(Worker);
    {inbox,InboxRef, {result, Result}} ->
      Worker#worker.jmgr#thread.pid ! {{worker, Worker},{result, Result}},
      worker_loop(Worker#worker{is_working = false});
    {inbox,InboxRef, {error,enotsock}} -> % kill, terminal disconnected
      kill(Worker);
    {inbox,InboxRef, {error, Reason}} ->
      ?DBGF("~p Received error from inbox: ~p\n",[Worker#worker.head#thread.ref, Reason]),
      worker_loop(Worker);
    {inbox,InboxRef, Data} ->
      ?DBGF("~p Received from worker's inbox: ~p\n",[Worker#worker.head#thread.ref,Data]),
      gen_server:cast(Worker#worker.bm,{inbox,Worker,Data}),
      worker_loop(Worker);
    {Sender, MsgRef, getworker} ->
      Sender ! {MsgRef,{worker,Worker}},
      worker_loop(Worker)
  end.

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
  receive
    die -> ok
  after
    0 ->
      case gen_tcp:recv(Worker#worker.socket, 0) of
        {ok, Data} ->
          Worker#worker.head#thread.pid ! {inbox,Worker#worker.inbox#thread.ref,packet:bin_decode(Data)},
          inbox_loop(Worker);
        {error, Reason} ->
          Worker#worker.head#thread.pid ! {inbox,Worker#worker.inbox#thread.ref,{error, Reason}},
          inbox_loop(Worker)
      end
  end.

-spec outbox_spawn() -> thread().
outbox_spawn() ->
  Ref = make_ref(),
  PID = spawn(fun() -> outbox() end),
  #thread{pid = PID, ref = Ref}.
  
-spec outbox() -> any().
outbox() ->
  receive
    die -> ok; % unsafe
    {worker, Worker} ->
      ?DBGF("~p Worker outbox is going to loop\n",[Worker#worker.head#thread.ref]),
      outbox_loop(Worker)
  end.

-spec outbox_loop(worker()) -> any().
outbox_loop(Worker) ->
  HeadRef = Worker#worker.head#thread.ref,
  receive
    die -> ok; % unsafe
    {HeadRef, MsgRef, send, Packet} ->
      case gen_tcp:send(Worker#worker.socket,packet:bin_encode(Packet)) of
        ok ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref,MsgRef,ok};
        {error, Reason} ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref, MsgRef,{error, Reason}}
      end,
      outbox_loop(Worker)
  end.

-spec kill(worker()) -> ok | error.
kill(Worker) ->
  Worker#worker.inbox#thread.pid ! die,
  Worker#worker.outbox#thread.pid ! die,
  ok.