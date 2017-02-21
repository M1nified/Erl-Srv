-module(worker_inbox).
-export([
  spawn/0
]).
-include("../headers/server_header.hrl").

-spec spawn() -> thread().
spawn() ->
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
    die ->
      ?DBGF("~p Worker inbox is shutting down\n",[Worker#worker.head#thread.ref]),
      ok
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
  