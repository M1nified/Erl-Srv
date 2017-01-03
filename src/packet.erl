-module(packet).
-export([
  bin_encode/1,
  bin_decode/1
]).

bin_encode(Term) ->
  term_to_binary(Term).

bin_decode(Binary) when is_binary(Binary) ->
  binary_to_term(Binary);
bin_decode(Binary) ->
  Binary.