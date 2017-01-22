-module(err).
-export([
  error/2
]).
-include("../headers/server_header.hrl").

error({error,Reason},{File,Line}) ->
  ?DBGOBJ([{Reason,{File,Line}}]).

