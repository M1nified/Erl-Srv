-module(worker_suite).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

jms_1() ->
  #jobs_manager_settings{
    readystorage = null,
    jobsmanager = null
  }.

worker__1_test() ->
  spawn(fun() -> timer:sleep(10),test_server:connect_worker() end),
  {ok, Socket, ListenSock} = test_server:get_socket(),
  JMS = jms_1(),
  WorkerThread = worker:spawn(Socket,JMS),
  #thread{pid = WorkerPid, ref = WorkerRef} = WorkerThread,
  WorkerPid = WorkerThread#thread.pid,
  WorkerRef = WorkerThread#thread.ref,

  gen_tcp:close(Socket),
  gen_tcp:close(ListenSock).

