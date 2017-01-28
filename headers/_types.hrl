-record(thread,{
  pid :: pid(),
  ref :: reference()
}).
-type thread() :: #thread{}.

-record(jm_state,{
  server_settings :: [proplist:property()],
  jobsmanager :: thread(),
  nodes :: pid(),
  todo = [] :: list(),
  free_workers = [] :: [worker()],
  bm :: module(),
  pending_tasks = #{} :: map() % {worker_ref, task}
}).
-type jm_state() :: #jm_state{}.

-record(display_connector,{
  connector :: thread(),
  socket :: socket()
}).
-type display_connector() :: #display_connector{}.

-record(worker,{
  head :: thread(),
  inbox :: thread(),
  outbox :: thread(),
  socket :: socket(),
  is_working = false :: boolean(),
  jmgr :: thread(),
  bm :: module()
}).
-type worker() :: #worker{}.

-type socket() :: gen_tcp:socket().