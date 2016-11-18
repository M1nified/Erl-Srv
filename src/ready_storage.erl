-module(ready_storage).
-export([
  spawn/0
]).
-include("../headers/settings.hrl").

-spec spawn() -> thread().
spawn() ->
  MyRef = make_ref(),
  Pid = spawn(fun() -> run(MyRef) end),
  ReStThread = #thread{
    pid = Pid, ref=MyRef
  },
  {ok, ReStThread}.

-spec run(reference()) -> any().
run(MyRef) ->
  listen(MyRef,#{ }).

-spec listen(reference(),map()) -> any().
listen(Buffer) ->
  receive
    {Sender,Ref,find,Key} ->
      Sender ! {Ref,maps:find(Key,Buffer)},
      listen(Buffer);
    {Sender,Ref,take,Key} ->
      try maps:take(Key,Buffer) of
        {Value, Map} ->
          Sender ! {Ref, Value},
          listen(Map);
        error ->
          Sender ! {Ref, error},
          listen(Buffer)
      catch
        _:Reason ->
          Sender ! {Ref, error}
      end
  end.
