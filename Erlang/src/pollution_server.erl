%%%-------------------------------------------------------------------
%%% @author HP 8-Gen
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2024 13:57
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("HP 8-Gen").

%% API
-export([start/0, stop/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2]).
-export([init/0, loop/1]).
-import(pollution, [create_monitor/0, add_station/3, add_value/5, remove_value/4, get_daily_mean/3, get_station_mean/3, get_one_value/4]).


start() -> register(pollutionServer, spawn(?MODULE, init, [])).

init() ->
  loop(create_monitor()).

%% main server loop
loop(Monitor) ->
  receive
    {request, Pid, add_station, {Name, Coordinates}} ->
      Pid ! {reply, ok},
      loop(pollution:add_station(Name, Coordinates, Monitor));
    {request, Pid, add_value, {Key, Date, Type, Value}} ->
      Pid ! {reply, ok},
      loop(pollution:add_value(Key, Date, Type, Value, Monitor));
    {request, Pid, remove_value, {Key, Date, Type}} ->
      Pid ! {reply, ok},
      loop(pollution:remove_value(Key, Date, Type, Monitor));
    {request, Pid, get_one_value, {Key, Date, Type}} ->
      Pid ! {reply, pollution:get_one_value(Key, Date, Type, Monitor)},
      loop(Monitor);
    {request, Pid, get_station_mean, {Key, Type}} ->
      Pid ! {reply, pollution:get_station_mean(Key, Type, Monitor)},
      loop(Monitor);
    {request, Pid, get_daily_mean, {Type, Date}} ->
      Pid ! {reply, pollution:get_daily_mean(Type, Date, Monitor)},
      loop(Monitor);
    {request, Pid, stop, {}} ->
      Pid ! {reply, ok}
end.

%% client
call(Message, Values) ->
  pollutionServer ! {request, self(), Message, Values},
  receive
    {reply, Reply} -> Reply
  end.

add_station(Name, Coordinates) -> call(add_station, {Name, Coordinates}).
add_value(Key, Date, Type, Value) -> call(add_value, {Key, Date, Type, Value}).
remove_value(Key, Date, Type) -> call(remove_value, {Key, Date, Type}).
get_one_value(Key, Date, Type) -> call(get_one_value, {Key, Date, Type}).
get_station_mean(Key, Type) -> call(get_station_mean, {Key, Type}).
get_daily_mean(Type, Date) -> call(get_daily_mean, {Type, Date}).
stop() -> call(stop, {}).
