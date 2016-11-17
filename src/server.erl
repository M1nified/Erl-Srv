-module(server).
-export([
  run/0
]).

run() -> % exceptions!!!!
  ReadyStoragePID = ready_storage:spawn(),
  DisplayConnectorPID = display_connector:spawn(),
  JobsManagerPID = jobs_manager:spawn().