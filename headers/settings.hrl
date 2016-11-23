-define(DEBUG, true).
-define(DBG(Msg), (case ?DEBUG of true -> (io:fwrite(Msg)); _ -> (null) end)).
-define(DBGF(Str, Arr), case ?DEBUG of true -> (io:fwrite(lists:flatten(io_lib:format(Str,Arr)))); _ -> (null) end).
-define(DBGOBJ(Array), (case ?DEBUG of true -> (io:fwrite(lists:flatten(io_lib:format("~p\n", Array)))); _ -> (null) end)).

-define(WORKER_PORT, 5678).
-define(DISPLAY_PORT, 8765).

-define(SERVER_ADDR, "localhost").

-define(WORKER_LISTEN_OPTIONS, [
                                  binary,
                                  {packet, 0},
                                  {active, false},
                                  {keepalive, true}
                                ]).
-define(DISPLAY_LISTEN_OPTIONS, [
                                  binary,
                                  {packet, 4},
                                  {active, false},
                                  {keepalive, true}
                                ]).
-define(WORKER_TERMINAL_OPTIONS,[
                                  binary,
                                  {packet, 0}
                                ]).

-define(MAX_FRAME_BUFFER, 1000).

-record(thread,{
  pid :: pid(),
  ref :: reference()
}).
-type thread() :: #thread{}.

-record(jobs_manager_settings,{
  jobsmanager :: thread()
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
  socket :: socket()
}).
-type worker() :: #worker{}.

-type socket() :: gen_tcp:socket().