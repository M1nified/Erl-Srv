-module(collector_suite).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/server_header.hrl").

spawn__1_test() ->
  {ok, _ReadyStorageThread} = collector:spawn().

kill__1_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(), 
  Rst#thread.pid ! {self(),Tid,kill},
  receive
    {Tid, ok} -> ok
  end,
  undefined = process_info(Rst#thread.pid).

put__1_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,put,{10,[1,1,1]}},
  receive
    {Tid, ok} -> ok;
    {Tid, {error,Reason}} -> ct:fail({2,Reason})
  end.

find__fail_1_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,find,key},
  receive
    {Tid, error} -> ok
  end.

find__ok_2_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,put,{key,[1,1,1]}},
  receive
    {Tid, ok} -> ok
  end,
  Tid2 = make_ref(),
  Rst#thread.pid ! {self(),Tid2,find,key},
  receive
    {Tid2, {ok,[1,1,1]}} -> ok
  end.

get_buffer__ok_1_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,get_buffer},
  receive
    {Tid, #{}} -> ok
  end.

get_buffer__ok_2_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,put,{key,[1,1,1]}},
  receive
    {Tid, ok} -> ok
  end,
  Tid2 = make_ref(),
  Rst#thread.pid ! {self(),Tid2,get_buffer},
  receive
    {Tid2, #{key := [1,1,1]}} -> ok
  end.

take__fail_1_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,take,key},
  receive
    {Tid, error} -> ok;
    {Tid, {error, Reason}} -> ct:fail(2,{error, Reason})
  end.

take__ok_2_test() ->
  {ok, Rst} = collector:spawn(),
  Tid = make_ref(),
  Rst#thread.pid ! {self(),Tid,put,{key,[1,1,1]}},
  receive
    {Tid, ok} -> ok
  end,
  Tid2 = make_ref(),
  Rst#thread.pid ! {self(),Tid2,take,key},
  receive
    {Tid2, {ok, [1,1,1]}} -> ok;
    {Tid2, error} -> ct:fail(2);
    {Tid2, {error, Reason}} -> ct:fail({3,Reason})
  end,
  Tid3 = make_ref(),
  Rst#thread.pid ! {self(),Tid3,get_buffer},
  receive
    {Tid3, #{}} -> ok
  end.
