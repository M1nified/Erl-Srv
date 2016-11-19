-module(err).
-export([
  error/2
]).
-include("../headers/settings.hrl").

error({error,Reason},{File,Line}) ->
  ?DBGOBJ([{Reason,{File,Line}}]).

