-module(terminal).
-export([
  start/1,
  start/2
]).
-include("../headers/server_header.hrl").

start(BehaviourModule) ->
  start(BehaviourModule,[]).
start(BehaviourModule,TerminalSettings) ->
  case gen_tcp:connect(
    proplists:get_value(server_address,TerminalSettings,?SERVER_ADDR_DEFAULT),
    proplists:get_value(worker_port,TerminalSettings,?TERMINAL_PORT_DEFAULT),
    ?TERMINAL_TCP_OPTIONS
  ) of
    {ok, Socket} ->
      connected(Socket,BehaviourModule);
    {error, Reason} ->
      ?DBGF("Connect error: ~p\n",[Reason])
      % start()
  end,
  ok.

connected(Socket,BehaviourModule) ->
  gen_server:start_link({local,BehaviourModule},BehaviourModule,[{socket,Socket}],[{debug,[log]}]),
  loop(Socket,BehaviourModule),
  gen_tcp:close(Socket).

loop(Socket,BehaviourModule) ->
  receive
    {tcp, Socket, Data} ->
      tcp_recv(Socket,BehaviourModule,packet:bin_decode(Data));
    {response, Cluster} ->
      gen_tcp:send(Socket,packet:bin_encode(Cluster)),
      loop(Socket,BehaviourModule);
    {exec, Command} ->
      recv_exec(Socket,BehaviourModule,Command)
  end.

recv_exec(_,_,shutdown) ->
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

tcp_recv(Socket,BehaviourModule,{call,{task, Task}}) ->
  try gen_server:call(BehaviourModule,packet:bin_decode(Task)) of
    Result ->
      Response = {result, Result},
      ?DBGF("Response: ~p\n",[Response]),
      gen_tcp:send(Socket,packet:bin_encode(Response)),
      loop(Socket,BehaviourModule)
  catch
    Error ->
      Response = {error, Error},
      ?DBGF("Terminal's behaviour module caused an error: ~p\n",[Error]),
      gen_tcp:send(Socket,packet:bin_encode(Response)),
      loop(Socket,BehaviourModule)
  end;
tcp_recv(Socket,BehaviourModule,{cast,Request}) ->
  gen_server:cast(BehaviourModule,packet:bin_decode(Request)),
  loop(Socket,BehaviourModule);
tcp_recv(_Socket,BehaviourModule,{terminate,Args}) when is_list(Args) ->
  ok = erlang:apply(gen_server,system_terminate,[BehaviourModule] ++ Args),
  ?DBG("Termination complete :P");

tcp_recv(_,_,Data) ->
  ?DBGF("tcp_recv - no match for Data = ~p\n",[Data]),
  trash.
