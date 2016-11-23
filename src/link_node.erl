-module(link_node).
-export([
  spawn/0
]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

spawn() ->
  spawn(fun() -> run(#{}) end).

-spec run(map()) -> any().
run(Vassals) ->
  receive
    Any ->
      recv(Vassals,Any)
  end.

-spec recv(map(),tuple()) -> any().
recv(Vassals,{From,Ref,reg,all,_}) ->
  spawn(fun() -> From ! {Ref,error,reserved_name_all} end),
  run(Vassals);
recv(Vassals,{From,Ref,reg,Name,Pid}) ->
  NewVassals = maps:put(Name,Pid,Vassals),
  spawn(fun() -> From ! {Ref,ok} end),
  run(NewVassals);
recv(Vassals,{From,Ref,get_vassals}) ->
  spawn(fun() -> From ! {Ref, Vassals} end),
  run(Vassals);
recv(Vassals,{From,Ref,forward,Message,to_all}) ->
  recv(Vassals,{From,Ref,forward,Message,maps:values(Vassals)});
recv(Vassals,{From,Ref,forward,Message,ToWhom}) ->
  ?DBGF("recv: ~p\n",[{Vassals,{From,Ref,Message,ToWhom}}]),
  case forward(Vassals,{From,Ref,Message,ToWhom}) of
    ok -> 
      ok;
    {error, Reason} ->
      spawn(fun() -> From ! {Ref,error,Reason} end);
    {warning, Reason} ->
      spawn(fun() -> From ! {Ref,warning,Reason} end)
  end,
  run(Vassals).

-spec forward(map(),tuple()) -> ok | {error, any()} | {warning, any()}.
forward(_,{From,Ref,Message,ToWhom}) when is_pid(ToWhom) ->
  spawn(fun() -> ToWhom ! {From,Ref,Message} end),
  ok;
forward(Vassals,{From,Ref,Message,ToWhom}) when is_atom(ToWhom) ->
  ?DBGF("forward is atom: ~p\n",[{Vassals,{From,Ref,Message,ToWhom}}]),
  try maps:get(ToWhom,Vassals) of
    ToPid ->
      forward(Vassals,{From,Ref,Message,ToPid}),
      ok
  catch
    _:Reason ->
      {error, {no_match_for_given_atom,Reason}}
  end;
forward(Vassals,{From,Ref,Message,ToWhom}) when is_list(ToWhom) ->
  Results = lists:map(fun(ToH) -> forward(Vassals,{From,Ref,Message,ToH}) end, ToWhom),
  case lists:flatlength(lists:filter(fun(Result) -> case Result of ok -> false; _ -> true end end,Results)) of
    0 -> ok;
    _ -> {warning, Results}
  end.

% forward_filter_



% LOCAL TESTS

recv_should_fail_if_register_as_all__test() ->
  Ref = make_ref(),
  Me = self(),
  spawn(fun() -> recv(#{},{Me,Ref,reg,all,Me}) end),
  receive
    {Ref,error,reserved_name_all} -> ok;
    _ -> ct:fail()
  end.

recv_should_forward_message_to_given_pid__test() ->
  Ref = make_ref(),
  Me = self(),
  spawn(fun() -> recv(#{},{Me,Ref,reg,me,Me}) end),
  receive
    {Ref,ok} -> ok;
    _ -> ct:fail()
  end.

recv_should_forward_message_to_registered_process__test() ->
  Ref = make_ref(),
  Me = self(),
  spawn(fun() -> recv(#{me=>Me},{Me,Ref,forward,{test,message,to},me}) end),
  receive
    {Me,Ref,{test,message,to}} -> ok;
    _ -> ct:fail()
  end.
  
recv_should_send_back_error_if_no_match__test() ->
  Ref = make_ref(),
  Me = self(),
  spawn(fun() -> recv(#{me=>Me},{Me,Ref,forward,{test,message,to},not_me}) end),
  receive
    {Ref,error,{no_match_for_given_atom,{badkey,not_me}}} -> ok;
    _ -> ct:fail()
  end.

recv_should_forward_to_multiple_threads__test() ->
  RefMe = make_ref(),
  Me = self(),
  Node2 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,{ok2,Message}} end end),
  Node3 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,{ok3,Message}} end end),
  spawn(fun() -> recv(#{node2=>Node2,node3=>Node3},{Me,RefMe,forward,message,[node2,node3]}) end),
  receive
    {RefMe,{ok2,message}} -> ok;
    {RefMe,{ok3,message}} -> ok;
    {RefMe,error,_} -> ct:fail()
  end,
  receive
    {RefMe,{ok2,message}} -> ok;
    {RefMe,{ok3,message}} -> ok;
    {RefMe,error,_} -> ct:fail()
  end.

recv_should_send_back_warning_if_no_match_present__test() ->
  RefMe = make_ref(),
  Me = self(),
  Node2 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,{ok2,Message}} end end),
  spawn(fun() -> recv(#{node2=>Node2},{Me,RefMe,forward,message,[node2,node3]}) end),
  receive
    {RefMe,{ok2,message}} -> ok;
    {RefMe,warning,_} -> ok; 
    Any1 -> ct:fail({1,Any1})
  end,
  receive
    {RefMe,{ok2,message}} -> ok;
    {RefMe,warning,_} -> ok; 
    Any2 -> ct:fail({2,Any2})
  end.


