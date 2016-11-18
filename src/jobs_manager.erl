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
    {ok, ReadyStorage} -> start(ReadyStorage)
  end.

-spec start(thread()) -> any().
start(ReadyStorage) ->
  JobsManager = jobs_manager_spawn(ReadyStorage),
  listen_go(#jobs_manager_settings{
    readystorage = ReadyStorage,
    jobsmanager = JobsManager
  }).

-spec jobs_manager_spawn(thread()) -> thread().
jobs_manager_spawn(ReadyStorage) ->
  Ref = make_ref(),
  #thread{
    pid = spawn(fun() -> jobs_manager(Ref,ReadyStorage) end),
    ref = Ref
  }.

jobs_manager(ManRef,ReadyStorage) ->
  receive
    {result, Data} -> 
      ok;
    {job_request, Worker} ->
      ok;
    {went_offline, Worker} ->
      ok
  end,
  jobs_manager(ManRef,ReadyStorage).

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
  worker:spawn(Socket,Settings),
  ok.

register_the_worker() ->
  ok.

%%
  
on_error({error,Where,Why}) ->
  {error,Where,Why}.