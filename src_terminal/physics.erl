-module(physics).

-export([
  step_cluster/3
]).
-include_lib("eunit/include/eunit.hrl").
-include("../headers/settings.hrl").

step_cluster(OldCluster, Time_0, Time) ->
  Delta_t = Time - Time_0,
  Temperature = temp(OldCluster#cluster.temperature,Time),
  Force = force(
    f_grav(?CONST_CLUSTER_MASS),
    f_conv(Temperature),
    f_fric(OldCluster#cluster.velocity,?CONST_CLUSTER_MASS),
    ?USER_DEFINED_FORCE
  ),
  Velocity = v_delta_t(OldCluster#cluster.velocity,Delta_t,Force,?CONST_CLUSTER_MASS),
  Position = x_delta_t(OldCluster#cluster.position,Delta_t,OldCluster#cluster.velocity),
  Particles = step_particles(OldCluster,Delta_t),
  OldCluster#cluster{
    position = Position,
    velocity = Velocity,
    temperature = Temperature,
    particles = Particles
  }.

step_particles(OldCluster,Delta_t) ->
  [Particle#particle{density=dens(Particle#particle.density,Delta_t,OldCluster#cluster.velocity)} || Particle <- OldCluster#cluster.particles].


% =============================================

% single cluster funcs

% x(t+dt) = x(t) + dt*v
-spec x_delta_t(list(),number(),list()) -> list().
x_delta_t(X_for_t0,Delta_t,V) ->
  ChangeOfX = [Dim * Delta_t || Dim <- V],
  add_lists(X_for_t0, ChangeOfX).

% v(t+dt) = v(t) + dt*F/maps
-spec v_delta_t(list(),number(),list(),number()) -> list().
v_delta_t(V_for_t0,Delta_t,Force,Mass) ->
  ChangeOfV = [Dim * Delta_t / Mass || Dim <- Force],
  add_lists(V_for_t0, ChangeOfV).

% F = F_grav + F_conv + F_fric + F_user
-spec force(list(),list(),list(),list()) -> list().
force(F_grav,F_conv,F_fric,F_user) ->
  add_lists([F_grav,F_conv,F_fric,F_user]).

% F_grav = c_grav * m (* e_down)
-spec f_grav(number()) -> list().
f_grav(Mass) ->
  [Dim * Mass * ?CONST_GRAV || Dim <- ?E_DOWN].

% F_conv
-spec f_conv(number()) -> list().
f_conv(Temp_for_t) ->
  [Dim * (?CONST_CONV_1/?TEMP_SIM - ?CONST_CONV_2/Temp_for_t) || Dim <- ?E_UP].

% T(t)
-spec temp(number(),number()) -> number().
temp(Temp_for_t0,Time) ->
  (Temp_for_t0 - ?TEMP_SIM) * math:pow(math:exp(1),-Time/?CONST_COOL) + ?TEMP_SIM.

% F_fric
-spec f_fric(list(),number()) -> list().
f_fric(V,Mass) ->
  V_abs = vector_len(V),
  [-?CONST_FRIC_1 * Mass * math:pow(V_abs,?CONST_FRIC_2) * Dim / V_abs || Dim <- V].
 
% Density
-spec dens(number(),number(),list()) -> number().
dens(Dens_for_t0,Delta_t,V) ->
  V_abs = vector_len(V),
  Dens_for_t0 - ?CONST_DISS * V_abs * Delta_t.


% HELPERS

add_lists([L1]) when is_list(L1) ->
  L1;
add_lists([L1,L2]) when is_list(L1) and is_list(L2) ->
  add_lists(L1,L2);
add_lists([L1,L2|Lists]) when is_list(L1) and is_list(L2) ->
  add_lists([add_lists(L1,L2)] ++ Lists).
add_lists_given_as_list_of_lists__test() ->
  [1,2,3] = add_lists([[1,2,3]]),
  [1,2,3] = add_lists([[0,1,2],[1,1,1]]),
  [1,2,3] = add_lists([[0,1,2],[1,1,1],[0,0,0]]),
  [1,2,3] = add_lists([[0,1,2],[1,1,1],[0,0,0],[0,0,0]]).

add_lists(L1,L2) ->
  lists:zipwith(fun(A,B) -> A+B end, L1, L2).
add_lists_given_as_two_arguments__test() ->
  [1,2,3] = add_lists([0,1,2],[1,1,1]).

multiply_lists(L1,L2) ->
  lists:zipwith(fun(A,B) -> A*B end, L1, L2).

vector_len(Vector) ->
  math:pow(lists:sum([math:pow(Dim,2) || Dim <- Vector]),1/2).
vector_len__test() ->
  Len = vector_len([1,2,3]),
  case erlang:abs(Len - 3.741657) < 0.0001 of
    true -> ok;
    false -> ct:fail({bad_score,Len})
  end.