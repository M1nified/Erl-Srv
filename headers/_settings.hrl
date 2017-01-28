-ifndef(DEBUG).
-define(DEBUG, true).
-endif.

-define(TERMINAL_PORT_DEFAULT, 5678).
-define(DISPLAY_PORT, 8765).

-define(SERVER_ADDR_DEFAULT, "localhost").

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
