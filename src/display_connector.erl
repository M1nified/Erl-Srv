-module(display_connector).
-export([
  spawn/1
]).
-include("../headers/settings.hrl").

-spec spawn(thread()) -> thread().
spawn(ReStThread) ->
  MyRef = make_ref(),
  Pid = erlang:spawn(fun() -> run(ReStThread) end),
  Me = #thread{
    pid = Pid, ref=MyRef
  },
  {ok, Me}.

-spec run(thread()) -> any().
run(ReStThread) ->
  Settings = #display_connector_settings{
    readystorage = ReStThread
  },
  listen_go(Settings).

-spec listen_go(display_connector_settings()) -> any().
listen_go(Settings) ->
  case gen_tcp:listen(
      ?DISPLAY_PORT,
      ?DISPLAY_LISTEN_OPTIONS
    ) of
    {ok, ListenSocket} -> accept_go(ListenSocket,Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_go(socket(),display_connector_settings()) -> any().
accept_go(ListenSocket,Settings) ->
  {ok, Port} = inet:port(ListenSocket),
  ?DBG(["Listening for display on port: ", integer_to_list(Port),"\n"]),
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} -> accept_ok(Socket, Settings);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_ok(socket(),display_connector_settings()) -> any().
accept_ok(Socket,Settings) ->
  listen(Socket,Settings),
  gen_tcp:close(Socket).

-spec listen(socket(),display_connector_settings()) -> any().
listen(Socket,Settings) ->
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      process_request(Settings,Data),
      listen(Socket,Settings);
    {error, Reason} ->
      err:error({error,Reason},{?FILE,?LINE})
  end.

-spec process_request(display_connector_settings(),any()) -> any().
process_request(Settings,Data) ->
  ok.
