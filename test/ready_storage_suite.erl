-module(ready_storage_suite).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

spawn__1_test() ->
  {ok, ReadyStorageThread} = ready_storage:spawn().



scenario_1_test() ->
  {ok, Rst} = ready_storage:spawn(),
  Tid = make_ref(), 
  Rst#thread.pid ! {self(),Tid,kill},
  receive
    {Tid, ok} -> ok
  end,
  undefined = process_info(Rst#thread.pid).