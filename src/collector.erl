-module(collector).
-export([
  spawn/0
]).
-include("../headers/settings.hrl").

-spec spawn() -> {ok,thread()}.
spawn() ->
  MyRef = make_ref(),
  Pid = spawn(fun() -> run(MyRef) end),
  ReStThread = #thread{
    pid = Pid, ref=MyRef
  },
  {ok, ReStThread}.

-spec run(reference()) -> any().
run(MyRef) ->
  listen(#{}).

-spec listen(map()) -> any().
listen(Buffer) ->
  receive
    {Sender, Ref, kill} ->
      Sender ! {Ref, ok};
    {Sender,Ref,put,{Key,Value}} ->
      try maps:put(Key,Value,Buffer) of
        Map -> 
          Sender ! {Ref, ok},
          listen(Map)
      catch
        _:Reason ->
          Sender ! {Ref, {error, Reason}},
          listen(Buffer)
      end;
    {Sender,Ref,find,Key} ->
      Sender ! {Ref,maps:find(Key,Buffer)},
      listen(Buffer);
    {Sender,Ref,take,Key} ->
      try maps:take(Key,Buffer) of
        {Value, Map} ->
          Sender ! {Ref, {ok, Value}},
          listen(Map);
        error ->
          Sender ! {Ref, error},
          listen(Buffer)
      catch
        _:Reason ->
          Sender ! {Ref, {error, Reason}},
          listen(Buffer)
      end;
    {Sender, Ref, get_buffer} ->
      Sender ! {Ref, Buffer},
      listen(Buffer);
    _ ->
      listen(Buffer)
  end.
