-module(jobs_manager).
-export([
  spawn/0
]).
-include("../headers/server_header.hrl").

spawn() ->
  Pid = erlang:spawn(fun wait_for_init_data/0),
  Me = #thread{
    pid = Pid, ref = make_ref()
  },
  {ok, Me}.

wait_for_init_data() ->
  ?DBG("jobs_manager is waiting for init data\n"),
  receive
    {error} -> ok;
    {behaviour_module, BehaviourModule} -> start(BehaviourModule)
  end.

-spec start(module()) -> any().
start(BehaviourModule) ->
  JobsManager = #thread{
    pid = self(),
    ref = make_ref()
  },
  JMS = #jobs_manager_settings{
    jobsmanager = JobsManager,
    nodes = link_node:spawn(),
    bm = BehaviourModule
  },
  gen_server:start_link({local,BehaviourModule},BehaviourModule,[],[{debug,[log]}]),
  erlang:spawn(fun() -> listen_go(JMS) end),
  jobs_manager(JMS).

% -spec jobs_manager_spawn() -> thread().
% jobs_manager_spawn() ->
%   Ref = make_ref(),
%   #thread{
%     pid = spawn(fun() -> jobs_manager(Ref) end),
%     ref = Ref
%   }.

-spec jobs_manager(jobs_manager_settings()) -> any().
jobs_manager(JMS_0) ->
  JMS = assign_jobs(JMS_0),
  receive
    Any ->
      recv(JMS,Any)
  after 0 ->
    % ?DBGF("JobsManager todo: ~p\n",[JMS#jobs_manager_settings.todo]),
    case lists:flatlength(JMS#jobs_manager_settings.todo) < 10 of
      true -> jobs_manager(get_next_job(JMS));
      false -> jobs_manager(JMS)
    end
  end,
  ok.

recv(JMS,{{worker,Worker},{result,Result}}) ->
  gen_server:cast(JMS#jobs_manager_settings.bm,{result,Result}),
  jobs_manager(JMS#jobs_manager_settings{free_workers = [JMS#jobs_manager_settings.free_workers ++ Worker]}),
  ok;
recv(JMS,{_From,_Ref,unleash, Worker}) ->
  jobs_manager(JMS#jobs_manager_settings{free_workers = [JMS#jobs_manager_settings.free_workers ++ Worker]}),
  ok;
recv(JMS,{_From,_Ref,remove, Worker}) ->
  JMS#jobs_manager_settings.nodes ! {self(),make_ref(),remove,Worker#worker.head#thread.ref},
  jobs_manager(JMS#jobs_manager_settings{free_workers = lists:filter(fun (W) -> W#worker.head#thread.ref /= Worker#worker.head#thread.ref end, JMS#jobs_manager_settings.free_workers)}),
  ok;
recv(JMS,{_From,_Ref,register_worker, Worker}) ->
  JMS#jobs_manager_settings.nodes ! {self(),make_ref(),reg,Worker#worker.head#thread.ref,Worker#worker.head#thread.pid},
  jobs_manager(JMS#jobs_manager_settings{free_workers = [JMS#jobs_manager_settings.free_workers ++ Worker]}),
  ok;
recv(JMS,Data) ->
  ?DBGF("JobsManager recv other: ~p ~p\n",[JMS,Data]),
  jobs_manager(JMS).

get_next_job(JMS) ->
      JMS#jobs_manager_settings{todo = lists:flatten([JMS#jobs_manager_settings.todo ++ [gen_server:call(JMS#jobs_manager_settings.bm,next_job)]])}.

assign_jobs(JMS) ->
  % ?DBGF("JobsManager assign_jobs, JMS: ~p\n",[JMS]),
  case pop_fst(JMS#jobs_manager_settings.todo) of
    {error, nothing_to_pop} ->
      JMS;
    {Task,Queue} ->
      % ?DBGF("JobsManager assign_jobs, task: ~p\n",[Task]),
      JMS2 = JMS#jobs_manager_settings{todo = Queue},
      case pop_fst(JMS2#jobs_manager_settings.free_workers) of
        {error, nothing_to_pop} ->
          JMS;
        {First, Rest} when is_list(Rest) ->
          First#worker.head#thread.pid ! {jms, assignment, {call, {task, Task}}},
          JMS2#jobs_manager_settings{free_workers = Rest}
      end
  end.

pop_fst([]) ->
  {error, nothing_to_pop};
pop_fst([First]) ->
  {First, []};
pop_fst([F|Tail]) ->
  {F,Tail}.

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

%% LOCAL UNIT TESTS
-include_lib("eunit/include/eunit.hrl").

pop_fst_should_pop_first_element__test() ->
  {1,[]} = pop_fst([1]),
  {somejob,[]} = pop_fst([somejob]),
  {1,[2,3,4,5]} = pop_fst([1,2,3,4,5]).