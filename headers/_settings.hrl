-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

-define(WORKER_PORT, 5678).
-define(DISPLAY_PORT, 8765).

-define(JOB,example_job).
-define(WORKER,example_worker).

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
