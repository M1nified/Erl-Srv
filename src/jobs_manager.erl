-module(jobs_manager).
-export([
  spawn/0
]).
-include("settings.hrl").

spawn() ->
  spawn(fun wait_for_init_data/0).

wait_for_init_data() ->
  receive
    {error} -> ok;
    {ReadyStorage} -> start(ReadyStorage)
  end.

-spec start(thread()) -> any().
start(ReadyStorage) ->
  listen_go({ReadyStorage}).

-spec listen_go(jobs_manager_settings()) -> any().
listen_go(Settings) ->
  case gen_tcp:listen(
      settings:stt(?WORKER_PORT),
      [
        binary,
        {packet, 0},
        {active, false},
        {keepalive, true}
      ]
    ) of
    {ok, ListenSock} -> listen_ok(ListenSock,Settings);
    {error, Reason} -> on_error({error,listen_go,Reason})
  end.

-spec listen_ok(socket(),jobs_manager_settings()) -> any().
listen_ok(ListenSock,Settings) ->
  accept_spawn(ListenSock,Settings),
  listen_go(Settings), % add exceptions
  gen_tcp:close(ListenSock).

-spec accept_spawn(socket(),jobs_manager_settings()) -> pid().
accept_spawn(ListenSock,Settings) ->
  spawn(fun() -> accept_go(ListenSock,Settings) end).

-spec accept_go(socket(),jobs_manager_settings()) -> any().
accept_go(ListenSock,Settings) ->
  case gen_tcp:accept(ListenSock) of
    {ok, Socket} -> accept_ok(Socket, Settings);
    {error, Reason} -> on_error({error,accept_go,Reason})
  end.

-spec accept_ok(socket(),jobs_manager_settings()) -> any().
accept_ok(Socket,Settings) ->
  io:fwrite("accept_ok\n"),
  worker_run(Socket,Settings),
  ok.

%% Single worker
-spec worker_run(socket(),jobs_manager_settings()) -> any().
worker_run(Socket,Settings) ->
  WorkerBoss = #thread{pid = self(),ref = make_ref()},
  InboxThread = worker_inbox_spawn(WorkerBoss,Socket,Settings),
  ChiefThread = worker_chief_spawn(WorkerBoss,Socket,Settings),
  register_the_worker(),
  ok.

-spec worker_inbox_spawn(thread(),socket(),jobs_manager_settings()) -> thread().
worker_inbox_spawn(Parent,Socket,Settings) ->
  Ref = make_ref(), 
  PID = spawn(fun() -> worker_inbox({Socket,Parent,Ref}) end),
  #thread{pid = PID, ref = Ref}.
-spec worker_inbox({socket(),thread(),reference()}) -> any().
worker_inbox({Socket,Parent,InboxRef}) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Parent#thread.pid ! {#thread{pid = self(),ref = InboxRef},Data};
    {error, Reason} ->
      Parent#thread.pid ! {#thread{pid = self(),ref = InboxRef},error,Reason}
  end.

-spec worker_chief_spawn(thread(),socket(),jobs_manager_settings()) -> thread().
worker_chief_spawn(Parent,Socket,Settings) ->
  Ref = make_ref(),
  PID = spawn(fun() -> worker_chief({Socket,Parent,Ref}) end),
  #thread{pid = PID, ref = Ref}.
-spec worker_chief({socket(),thread(),reference()}) -> any().
worker_chief({Socket,Parent,ChiefRef}) ->
  PRef = Parent#thread.ref,
  receive
    {PRef, send, Packet} ->
      case gen_tcp:send(Socket,Packet) of
        ok ->
          Parent#thread.pid ! {#thread{pid = self(),ref = ChiefRef}, send_complete};
        {error, Reason} ->
          Parent#thread.pid ! {#thread{pid = self(),ref = ChiefRef}, error, Reason}
      end,
      worker_chief({Socket,Parent,ChiefRef})
  end.

register_the_worker() ->
  ok.


  
on_error({error,Where,Why}) ->
  {error,Where,Why}.