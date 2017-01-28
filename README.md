# Erlang-Concurrent-TCP-Server

[![Build Status](https://travis-ci.org/M1nified/Erlang-Srv.svg?branch=master)](https://travis-ci.org/M1nified/Erlang-Srv)

## Requirements

- Erlang/OTP ^19.0

## Server interface

The main module of the server is called `tcp_cc`. Its exported functions are used to initiate both server and terminal processes.   

To start server use:
```erlang
tcp_cc:start_server(
  process_name,                 % for future communication (not implemented yet)
  behaviour_module_for_server,
  [server_settings]             % optional proplist
).
```
   
To start terminal use:
```erlang
tcp_cc:start_terminal(
  process_name,                 % for future communication (not implemented yet)
  behaviour_module_for_terminal,
  [terminal_settings]           % optional proplist
).
```

### Settings description

Both `server_settings` and `terminal_settings` are of type `[proplist:property()]` and are optional.

`server_settings` list accepts the following keys:
- `terminal_port`, of type `inet:port_number()`

`terminal_settings` list accepts the following keys:
- `terminal_port`, of type `inet:port_number()`
- `server_address`, of type `inet:socket_address() | inet:hostname()`

Please look into [main_server.erl](example_1/main_server.erl) and [main_terminal.erl](example_1/main_terminal.erl) for better understanding.


# Dev

## Executables

- `_test.sh` / `_test.bat` runs tests
