-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

-define(TERMINAL_PORT_DEFAULT, 5678).

-ifndef(WORKER_PORT).
-define(WORKER_PORT, 5678).
-endif.

-ifndef(DISPLAY_PORT).
-define(DISPLAY_PORT, 8765).
-endif.

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
-define(TERMINAL_TCP_OPTIONS,[
                                  binary,
                                  {packet, 0}
                                ]).
