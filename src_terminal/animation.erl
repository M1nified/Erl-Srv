-module(animation).
-export([
  spawn_cluster/2
]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

-spec spawn_cluster(source(),integer()) -> cluster().
spawn_cluster(Source,Time_0) ->
  Particles = [rand_particle(Source) || _ <- lists:seq(0,?PPC)],
  #cluster{
    particles = Particles,
    source_id = Source#source.source_id,
    time = Time_0
  }.

-spec rand_particle(source()) -> particle().
rand_particle(Source) ->
  Position = rand_position(Source#source.position,Source#source.size),
  Velocity = rand_velocity(Source#source.velocity_range),
  #particle{
    position = Position,
    velocity = Velocity,
    temperature = 0
  }.

rand_position([P1,P2,P3],[S1,S2,S3]) ->
  [
    P1 + rand:uniform(round(2*S1)) - round(S1),
    P2 + rand:uniform(round(2*S2)) - round(S2),
    P3 + rand:uniform(round(2*S3)) - round(S3)
  ].

rand_velocity(Ranges) ->
  [ Bot + rand:uniform() * math:abs(Top-Bot) || [Bot,Top] <- Ranges].