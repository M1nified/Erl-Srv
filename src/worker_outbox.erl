-module(worker_outbox).
-export([
  spawn/0
]).
-include("../headers/server_header.hrl").

-spec spawn() -> thread().
spawn() ->
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
    die -> 
      ?DBGF("~p Worker outbox is shutting down\n",[Worker#worker.head#thread.ref]),
      ok; % unsafe
    {HeadRef, MsgRef, send, Packet} ->
      case gen_tcp:send(Worker#worker.socket,packet:bin_encode(Packet)) of
        ok ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref,MsgRef,ok};
        {error, Reason} ->
          Worker#worker.head#thread.pid ! {outbox,Worker#worker.inbox#thread.ref, MsgRef,{error, Reason}}
      end,
      outbox_loop(Worker)
  end.
