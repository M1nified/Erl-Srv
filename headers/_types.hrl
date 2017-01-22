-record(thread,{
  pid :: pid(),
  ref :: reference()
}).
-type thread() :: #thread{}.

-record(jobs_manager_settings,{
  jobsmanager :: thread(),
  nodes :: pid(),
  todo = [] :: list(),
  free_workers = [] :: list()
}).
-type jobs_manager_settings() :: #jobs_manager_settings{}.

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
  jmgr :: thread()
}).
-type worker() :: #worker{}.

-type socket() :: gen_tcp:socket().