-module(display_connector).
-export([
  spawn/1
]).
-include("../headers/settings.hrl").

-spec spawn(thread()) -> thread().
spawn(ReStThread) ->
  MyRef = make_ref(),
  Pid = erlang:spawn(fun() -> run(ReStThread,MyRef) end),
  Me = #thread{
    pid = Pid, ref=MyRef
  },
  {ok, Me}.

-spec run(thread(),reference()) -> any().
run(ReStThread,MyRef) ->
  DisplConn = #display_connector{
    readystorage = ReStThread,
    connector = #thread{pid=self(),ref=MyRef}
  },
  listen_go(DisplConn).

-spec listen_go(display_connector()) -> any().
listen_go(DisplConn) ->
  case gen_tcp:listen(
      ?DISPLAY_PORT,
      ?DISPLAY_LISTEN_OPTIONS
    ) of
    {ok, ListenSocket} -> accept_go(ListenSocket,DisplConn);
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_go(socket(),display_connector()) -> any().
accept_go(ListenSocket,DisplConn) ->
  {ok, Port} = inet:port(ListenSocket),
  ?DBG(["Listening for display on port: ", integer_to_list(Port),"\n"]),
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} -> accept_ok(DisplConn#display_connector{socket=Socket});
    {error, Reason} -> err:error({error,Reason},{?FILE,?LINE})
  end.

-spec accept_ok(display_connector()) -> any().
accept_ok(DisplConn) ->
  listen(DisplConn),
  gen_tcp:close(DisplConn#display_connector.socket).

-spec listen(display_connector()) -> any().
listen(DisplConn) ->
  case gen_tcp:recv(DisplConn#display_connector.socket,0) of
    {ok, Data} ->
      process_request(DisplConn,Data),
      listen(DisplConn);
    {error, Reason} ->
      err:error({error,Reason},{?FILE,?LINE})
  end.

-spec process_request(display_connector(),any()) -> any().
process_request(DisplConn,Data) ->
  ok.
