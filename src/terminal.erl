-module(terminal).
-export([
  init/0
]).
-include("../headers/settings.hrl").

init() ->
  case gen_tcp:connect(?SERVER_ADDR, ?WORKER_PORT, ?WORKER_TERMINAL_OPTIONS) of
    {ok, Socket} ->
      connected(Socket);
    {error, Reason} ->
      ?DBGF("Connect error: ~p\n",[Reason])
      % init()
  end,
  ok.

connected(Socket) ->
  gen_server:start_link({local,?JOB},?JOB,[{socket,Socket}],[{debug,[log]}]),
  loop(Socket),
  gen_tcp:close(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      tcp_recv(Socket,packet:bin_decode(Data));
    {response, Cluster} ->
      gen_tcp:send(Socket,packet:bin_encode(Cluster)),
      loop(Socket);
    {exec, Command} ->
      recv_exec(Socket,Command)
  end.

recv_exec(_,shutdown) ->
  ?DBG("Recieved exec(shutdown). Going down...\n"),
  ok.

% tcp_recv(Socket,{WorkerReference,new_cluster,Source,Time_0,FramesToCount}) when is_integer(FramesToCount)->
%   Cluster = animation:spawn_cluster(Source,Time_0),
%   TerminalPID = self(),
%   spawn(fun() -> animate_cluster(TerminalPID,Cluster,FramesToCount) end),
%   loop(Socket);

% tcp_recv(Socket,{WorkerReference,add_cluster, Cluster, FramesToCount}) when is_integer(FramesToCount) ->
%   TerminalPID = self(),
%   spawn(fun() -> animate_cluster(TerminalPID,Cluster,FramesToCount) end),
%   loop(Socket);

tcp_recv(Socket,{call,{task, Task}}) ->
  Response = {result, gen_server:call(?JOB,packet:bin_decode(Task))},
  ?DBGF("Response: ~p\n",[Response]),
  gen_tcp:send(Socket,packet:bin_encode(Response)),
  loop(Socket);
tcp_recv(Socket,{cast,Request}) ->
  gen_server:cast(?JOB,packet:bin_decode(Request)),
  loop(Socket);
tcp_recv(_Socket,{terminate,Args}) when is_list(Args) ->
  ok = erlang:apply(gen_server,system_terminate,[?JOB] ++ Args),
  ?DBG("Termination complete :P");

tcp_recv(_,Data) ->
  ?DBGF("tcp_recv - no match for Data = ~p\n",[Data]),
  trash.
