-module(parser_suite).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").
-include("../headers/asserts.hrl").

json_to_map__1_test() ->
  ok.

map_to_json__1_test() ->
  ?match("{\"a\":2,\"b\":3}",parser:map_to_json(#{a=>2,b=>3})).