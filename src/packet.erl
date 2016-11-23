-module(packet).
-export([
  bin_encode/1,
  bin_decode/1
]).

bin_encode(Tuple) when is_tuple(Tuple) ->
  term_to_binary(Tuple).

bin_decode(Binary) when is_binary(Binary) ->
  binary_to_term(Binary).