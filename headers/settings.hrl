-define(WORKER_PORT, 9876).
-define(DISPLAY_PORT, 8765).

-define(MAX_FRAME_BUFFER, 1000).

-define(DEBUG, true).
-define(DBG(Msg), (case ?DEBUG of true -> (io:fwrite(Msg)); _ -> (null) end)).

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