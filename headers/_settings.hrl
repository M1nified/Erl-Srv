-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

-ifndef(WORKER_PORT).
-define(WORKER_PORT, 5678).
-endif.
-ifndef(DISPLAY_PORT).
-define(DISPLAY_PORT, 8765).
-endif.

-define(JOB,example_job).
-define(WORKER,example_worker).

-define(SERVER_ADDR, "localhost").
% -define(SERVER_ADDR, "149.156.207.22"). 

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
