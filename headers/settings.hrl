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
-define(TIME_STEP,10).%ms

-define(CONST_GRAV, 9.8).
-define(CONST_CONV_1, 100).
-define(CONST_CONV_2, 100).
-define(CONST_COOL, 100).
-define(CONST_FRIC_1, 100).
-define(CONST_FRIC_2, 100).
-define(CONST_DISS, 100).
-define(CONST_CLUSTER_MASS, 1).

-define(USER_DEFINED_FORCE, [0,0,0]).

-define(TEMP_SIM, 100).

-define(E_UP,[0,0,1]).
-define(E_DOWN,[0,0,-1]).

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

-record(particle,{
  position :: list(),
  density :: number()
}).
-type particle() :: #particle{}.

-record(cluster,{
  position :: list(),
  velocity :: list(),
  temperature :: number(),
  particles :: [particle()]
}).
-type cluster() :: #cluster{}.