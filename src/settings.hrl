-define(WORKER_PORT, 9876).
-define(DISPLAY_PORT, 8765).

-record(thread,{
  pid :: pid(),
  ref :: reference()
}).
-type thread() :: #thread{}.

-record(jobs_manager_settings,{
  readystorage :: thread(),
  jobsmanager :: thread()
}).
-type jobs_manager_settings() :: #jobs_manager_settings{}.

-type socket() :: gen_tcp:socket().