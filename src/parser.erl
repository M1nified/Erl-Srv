-module(parser).
-export([
  json_to_map/1,
  map_to_json/1
]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/asserts.hrl").
-include("../headers/settings.hrl").

json_to_map(Json) ->
    ok.

map_to_json(Map) when is_map(Map) ->
  "{" ++ lists:flatten(lists:join(",",lists:map(fun map_to_json_elem/1 , maps:to_list(Map)))) ++ "}".

map_to_json_elem({Key, Value}) ->
  lists:flatten(io_lib:format("\"~p\":~s", [Key, map_to_json_value(Value)])).

map_to_json_value(Value) when is_number(Value) ->
  lists:flatten(io_lib:format("~p",[Value]));
map_to_json_value(Value) ->
  lists:flatten(io_lib:format("~p",[Value])).

map_to_json_2(List,String) ->
  ok.
  


%% local tests

map_to_json_elem__1_test() ->
  ?match("\"a\":2",map_to_json_elem({a,2})).

map_to_json_value__1_test() ->
  ?match("1",map_to_json_value(1)),
  ?match("\"1\"",map_to_json_value("1")).