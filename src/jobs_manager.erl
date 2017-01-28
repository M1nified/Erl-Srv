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
    {{behaviour_module, BehaviourModule},{settings, SettingsMap}} -> start(BehaviourModule,SettingsMap)
  end.

-spec start(module(),map()) -> any().
start(BehaviourModule,SettingsMap) ->
  JobsManager = #thread{
    pid = self(),
    ref = make_ref()
  },
  JMS = #jm_state{
    jobsmanager = JobsManager,
    nodes = link_node:spawn(),
    bm = BehaviourModule,
    server_settings = SettingsMap
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

-spec jobs_manager(jm_state()) -> any().
jobs_manager(JMS_0) ->
  JMS = assign_jobs(JMS_0),
  receive
    Any ->
      recv(JMS,Any)
  after 1 ->
    % ?DBGF("JobsManager todo: ~p\n",[JMS#jm_state.todo]),
    case lists:flatlength(JMS#jm_state.todo) < 10 of
      true -> jobs_manager(get_next_job(JMS));
      false -> jobs_manager(JMS)
    end
  end,
  ok.

recv(JMS,{{worker,Worker},{result,Result}}) ->
  gen_server:cast(JMS#jm_state.bm,{result,Result}),
  jobs_manager(free_worker(JMS,Worker)),
  ok;
recv(JMS,{worker_shutdown, {worker, Worker}, {reason, Reason}}) ->
  ?DBGF("Worker (~p) reported shutdown, reason: ~p\n",[Worker#worker.head#thread.ref,Reason]),
  JMS_removed = remove_worker(JMS,Worker),
  jobs_manager(JMS_removed);
recv(JMS,{_From,_Ref,unleash, Worker}) ->
  jobs_manager(JMS#jm_state{free_workers = [JMS#jm_state.free_workers ++ Worker]}),
  ok;
% recv(JMS,{_From,_Ref,remove, Worker}) ->
%   jobs_manager(remove_worker(JMS,Worker)),
%   ok;
recv(JMS,{_From,_Ref,register_worker, Worker}) ->
  JMS#jm_state.nodes ! {self(),make_ref(),reg,Worker#worker.head#thread.ref,Worker#worker.head#thread.pid},
  % ?DBGF("\n\nfree workers: ~p\n\n",[JMS#jm_state.free_workers]),
  jobs_manager(free_worker(JMS,Worker)),
  ok;
recv(JMS,Data) ->
  % ?DBGF("JobsManager recv other: ~p \~\p\n",[JMS,Data]),
  ?DBGF("JobsManager recv other: ~p\n",[Data]),
  jobs_manager(JMS).

get_next_job(JMS) ->
  JMS#jm_state{
    todo = lists:flatten([JMS#jm_state.todo ++ [gen_server:call(JMS#jm_state.bm,next_job)]])
  }.

-spec remove_worker(jm_state(),worker()) -> jm_state().
remove_worker(JMS_0,Worker) ->
  JMS_0#jm_state.nodes ! {self(),make_ref(),remove,Worker#worker.head#thread.ref},
  JMS = unbusy_worker(JMS_0,Worker),
  % ?DBGF("remove_worker, todo: ~p\n~p",[JMS_0#jm_state.todo,JMS#jm_state.todo]),
  JMS#jm_state{
    free_workers = lists:filter(
      fun (W) -> W#worker.head#thread.ref /= Worker#worker.head#thread.ref end,
      JMS#jm_state.free_workers
    )
  }.

-spec free_worker(jm_state(),worker()) -> jm_state().
free_worker(JMS,Worker) ->
  PendingTasks = maps:remove(Worker#worker.head#thread.ref, JMS#jm_state.pending_tasks),
  FreeWorkers = lists:filter(fun (W) -> W#worker.head#thread.ref /= Worker#worker.head#thread.ref end, JMS#jm_state.free_workers) ++ [Worker],
  JMS#jm_state{
    free_workers = FreeWorkers,
    pending_tasks = PendingTasks
  }.

unbusy_worker(JMS,Worker) ->
  case maps:get(Worker#worker.head#thread.ref, JMS#jm_state.pending_tasks, has_no_assignment) of
    has_no_assignment ->
      PendingTasks = JMS#jm_state.pending_tasks,
      TODO = JMS#jm_state.todo;
    Task ->
      PendingTasks = maps:remove(Worker#worker.head#thread.ref, JMS#jm_state.pending_tasks),
      TODO = lists:flatten([Task,JMS#jm_state.todo])
  end,
  JMS#jm_state{
    pending_tasks=PendingTasks, 
    todo=TODO
  }.



assign_jobs(JMS) ->
  % ?DBGF("JobsManager assign_jobs, JMS: ~p\n",[JMS]),
  case pop_fst(JMS#jm_state.todo) of
    {error, nothing_to_pop} ->
      JMS;
    {Task,Queue} ->
      % ?DBGF("JobsManager assign_jobs, task: ~p\n",[Task]),
      JMS2 = JMS#jm_state{todo = Queue},
      case pop_fst(JMS2#jm_state.free_workers) of
        {error, nothing_to_pop} ->
          JMS;
        {First, Rest} when is_list(Rest) ->
          % ?DBGF("JobsManager before pop: ~p\nafter: ~p\n",[JMS#jm_state.todo,JMS2#jm_state.todo]),
          First#worker.head#thread.pid ! {jms, assignment, {call, {task, Task}}},
          JMS2#jm_state{free_workers = Rest, pending_tasks=maps:put(First#worker.head#thread.ref,Task,JMS2#jm_state.pending_tasks)}
      end
  end.

pop_fst([]) ->
  {error, nothing_to_pop};
pop_fst([First]) ->
  {First, []};
pop_fst([F|Tail]) ->
  {F,Tail}.

-spec listen_go(jm_state()) -> any().
listen_go(Settings) ->
  case gen_tcp:listen(
      % ?WORKER_PORT,
      proplists:get_value(worker_port,Settings#jm_state.server_settings,?TERMINAL_PORT_DEFAULT),
      ?WORKER_LISTEN_OPTIONS
    ) of
    {ok, ListenSock} -> listen_ok(ListenSock,Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec listen_ok(socket(),jm_state()) -> any().
listen_ok(ListenSock,Settings) ->
  {ok, Port} = inet:port(ListenSock),
  ?DBG(["Listening for workers on port: ", integer_to_list(Port),"\n"]),
  accept_go(ListenSock,Settings),
  % listen_go(Settings), % add exceptions
  gen_tcp:close(ListenSock).

% -spec accept_spawn(socket(),jm_state()) -> pid().
% accept_spawn(ListenSock,Settings) ->
%   spawn(fun() -> accept_go(ListenSock,Settings) end).

-spec accept_go(socket(),jm_state()) -> any().
accept_go(ListenSock,Settings) ->
  ?DBG("Waiting for worker to accept...\n"),
  case gen_tcp:accept(ListenSock) of
    {ok, Socket} -> 
      accept_ok(Socket, Settings),
      accept_go(ListenSock, Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_ok(socket(),jm_state()) -> any().
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