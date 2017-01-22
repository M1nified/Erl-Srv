-module(link_node_suite).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/server_header.hrl").

should_spawn_test() ->
  case link_node:spawn() of
    Node when is_pid(Node) -> ok;
    _ -> ct:fail()
  end.

should_register_process_test() ->
  LN = link_node:spawn(),
  Ref = make_ref(),
  LN ! {self(),Ref,reg,proc1,self()},
  receive
    {Ref, ok} -> ok;
    _ -> ct:fail()
  end.

should_register_process_and_get_vassals_test() ->
  LN = link_node:spawn(),
  Ref = make_ref(),
  Self = self(),
  LN ! {Self,Ref,reg,proc1,Self},
  receive
    {Ref, ok} -> ok;
    _ -> ct:fail()
  end,
  LN ! {Self,Ref,get_vassals},
  receive
    {Ref, Vassals} ->
      try maps:get(proc1,Vassals) of
        Self -> ok
      catch
        _:_ -> ct:fail(did_not_add_to_map)
      end;
    _ -> ct:fail(bad_response)
  after
    100 -> ct:fail(timeout)
  end.

should_pass_message_one_to_one_test() ->
  LN = link_node:spawn(),
  Ref = make_ref(),
  Self = self(),
  LN ! {Self,Ref,forward,msg1,Self},
  receive
    msg1 -> ok;
    _ -> ct:fail()      
  end.

should_pass_message_to_registered_procc_test() ->
  LN = link_node:spawn(),
  Me = self(),
  Ref1 = make_ref(),
  Ref2 = make_ref(),
  LN ! {Me,Ref1,reg,me,self()},
  receive
    {Ref1, ok} -> ok;
    _ -> ct:fail(unable_to_register)
  end,
  _ = spawn(fun() -> forward(LN,Ref2,msg1,me) end),
  % ?DBGF("Me sender: ~p\n",[Me]),
  % LN ! {Me,Ref2,forward,msg1,me},
  receive
    msg1 -> ok;
    {_,EorW,Reason} -> ct:fail({2,EorW,Reason});
    _ -> ct:fail(wrong_message)
  after
    500 ->
      ct:fail(timeout)
  end.

should_pass_message_to_multiple_proccs_test() ->
  LN = link_node:spawn(),
  Me = self(),
  MeRef = make_ref(),
  Node2 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,ok2,Message} end end),
  Node3 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,ok3,Message} end end),
  LN ! {Me,0,reg,node2,Node2},
  LN ! {Me,0,reg,node3,Node3},
  LN ! {Me,MeRef,forward,{Me,MeRef,info_to_pass},[node2,node3]},
  receive
    {MeRef,ok2,info_to_pass} -> ok
  end,
  receive
    {MeRef,ok3,info_to_pass} -> ok
  end.

  
should_pass_message_to_all_registered_procs_test() ->
  LN = link_node:spawn(),
  Me = self(),
  MeRef = make_ref(),
  Node2 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,{ok2,Message}} end end),
  Node3 = spawn(fun() -> receive {From,Ref,Message} -> From ! {Ref,{ok3,Message}} end end),
  LN ! {Me,0,reg,node2,Node2},
  LN ! {Me,0,reg,node3,Node3},
  LN ! {Me,MeRef,forward,{Me,MeRef,info_to_pass},to_all},
  receive
    {MeRef,{ok2,info_to_pass}} -> ok
  end,
  receive
    {MeRef,{ok3,info_to_pass}} -> ok
  end.

% HELPERS

forward(LinkNode,Ref,Message,ToWhom) ->
  LinkNode ! {self(),Ref,forward,Message,ToWhom}.