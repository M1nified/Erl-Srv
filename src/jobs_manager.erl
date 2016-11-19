-module(jobs_manager).
-export([
  spawn/0
]).
-include("../headers/settings.hrl").

spawn() ->
  Pid = erlang:spawn(fun wait_for_init_data/0),
  Me = #thread{
    pid = Pid, ref = make_ref()
  },
  {ok, Me}.

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

-spec jobs_manager(reference(),thread()) -> any().
jobs_manager(ManRef,ReadyStorage) ->
  receive
    {result, Data} -> 
      ok;
    {job_request, Worker} ->
      ok;
    {went_offline, Worker} ->
      ok;
    {register_worker, Worker} ->
      ok
  end,
  jobs_manager(ManRef,ReadyStorage).

-spec listen_go(jobs_manager_settings()) -> any().
listen_go(Settings) ->
  case gen_tcp:listen(
      ?WORKER_PORT,
      ?WORKER_LISTEN_OPTIONS
    ) of
    {ok, ListenSock} -> listen_ok(ListenSock,Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec listen_ok(socket(),jobs_manager_settings()) -> any().
listen_ok(ListenSock,Settings) ->
  {ok, Port} = inet:port(ListenSock),
  ?DBG(["Listening for workers on port: ", integer_to_list(Port),"\n"]),
  accept_go(ListenSock,Settings),
  % listen_go(Settings), % add exceptions
  gen_tcp:close(ListenSock).

% -spec accept_spawn(socket(),jobs_manager_settings()) -> pid().
% accept_spawn(ListenSock,Settings) ->
%   spawn(fun() -> accept_go(ListenSock,Settings) end).

-spec accept_go(socket(),jobs_manager_settings()) -> any().
accept_go(ListenSock,Settings) ->
  ?DBG("Waiting for worker to accept...\n"),
  case gen_tcp:accept(ListenSock) of
    {ok, Socket} -> 
      accept_ok(Socket, Settings),
      accept_go(ListenSock, Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_ok(socket(),jobs_manager_settings()) -> any().
accept_ok(Socket,Settings) ->
  io:fwrite("accept_ok\n"),
  worker:spawn(Socket,Settings),
  ok.

register_the_worker() ->
  ok.

%% LOCAL UNIT TESTS
-include_lib("eunit/include/eunit.hrl").
