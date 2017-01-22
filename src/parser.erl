-module(parser).
-export([
  % json_to_map/1,
  % map_to_json/1,
  float_to_bin/1
]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/asserts.hrl").
-include("../headers/server_header.hrl").

float_to_bin(Value) when is_float(Value) ->
  <<Value:32/little-float>>;
float_to_bin(Value) when is_number(Value) ->
  float_to_bin(erlang:float(Value));
float_to_bin(List) when is_list(List) ->
  erlang:list_to_binary([float_to_bin(Value) || Value <- List]).
