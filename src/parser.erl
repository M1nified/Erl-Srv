-module(parser).
-export([
  json_to_map/1,
  map_to_json/1
]).
-include("../headers/settings.hrl").

json_to_map(Json) ->
    ok.

map_to_json(Map) when is_map(Map) ->
  "{" ++ lists:flatten([ ["\"",Key,"\":\"",Value,"\""] || {Key,Value} <- maps:to_list(Map)]) ++ "}".

map_to_json_2(List,String) ->
  ok.
  
