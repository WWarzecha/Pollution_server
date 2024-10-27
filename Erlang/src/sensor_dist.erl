%%%-------------------------------------------------------------------
%%% @author HP 8-Gen
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2024 12:58
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("HP 8-Gen").

%% API
-export([get_random_locations/1, dist/2, find_for_person/2, find_for_person/3, find_closest/2, compare_speeds/4, find_closest_parallel/2, test/0]).

get_random_locations(N) -> [{rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,N)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X1-X2, 2) + math:pow(Y1-Y2, 2)).

find_for_person(PersonLocation, SensorsLocations) -> lists:min([{dist(PersonLocation, Z), PersonLocation, Z} || Z <- SensorsLocations]).
find_for_person(PersonLocation, SensorsLocations, ParentPID) -> ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest(PeopleLocations, SensorsLocations) -> lists:min([find_for_person(X, SensorsLocations) || X <- PeopleLocations]).
find_closest_parallel(PeopleLocations, SensorsLocations) ->
  [spawn(?MODULE, find_for_person, [X, SensorsLocations, self()]) || X <- PeopleLocations],
  lists:min([receive N -> N end || _ <- PeopleLocations]).

compare_speeds(List1, List2, Fun1, Fun2) -> io:format("Result:~n ~w,  ~w~n", [element(1,timer:tc(Fun1, [List1, List2])), element(1,timer:tc(Fun2, [List1, List2]))]).

test() ->
  S = get_random_locations(1000),
  P = get_random_locations(5000),
  compare_speeds(P, S, fun(X,Y) -> find_closest(X, Y) end, fun(X,Y) -> find_closest_parallel(X, Y) end).