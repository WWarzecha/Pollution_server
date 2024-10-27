%%%-------------------------------------------------------------------
%%% @author HP 8-Gen
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. kwi 2024 20:18
%%%-------------------------------------------------------------------
-module(pollution).
-author("HP 8-Gen").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_closest_stations/1]).



-record(station, {name, coordinates}).
-record(reading, {type, value, date}).


create_monitor() -> #{}.

add_station(N, C, M) ->
  Flag = is_present_station(N, C, M),
  case Flag of
    true -> {error, "Station already exists"};
    _ -> M#{#station{name = N, coordinates = C} => []}
  end.

is_present_station(N, C, Monitor) ->
  Stations = maps:keys(Monitor),
  Flag1 = lists:any(fun(#station{name = Name}) -> Name == N end, Stations),
  Flag2 = lists:any(fun(#station{coordinates = Coords}) -> Coords == C end, Stations),
  case {Flag1, Flag2} of
    {false, false} -> false;
    _ -> true
  end.


get_station(Key, Monitor) ->
  Stations = maps:keys(Monitor),
  case Stations of
    [] -> 0;
    _ -> L1 = lists:filter(fun(#station{name = Name}) -> Name == Key end, Stations),
      L2 = lists:filter(fun(#station{coordinates = Coordinates}) -> Coordinates == Key end, Stations),
      L3 = L1 ++ L2,
      [X|_] = L3,
      X
  end.


is_reading_present(Station, Reading, Monitor) ->
%%  Station = get_station(Key, Monitor),
  Readings = maps:get(Station, Monitor, []),
  lists:any(fun(X) -> compare_readings(Reading, X) end, Readings).

compare_readings(R1, R2) ->
  #reading{type = Type1, date = Date1} = R1,
  #reading{type = Type2, date = Date2} = R2,
  case {Type1, Type2, Date1, Date2} of
    {Type1, Type1, Date1, Date1} -> true;
    _ -> false
  end.

add_value(Key, Date, Type, Value, Monitor) ->
  Flag = is_present_station(Key, Key, Monitor),
  case Flag of
    false -> {error, station_does_not_exist};
    _ ->  Station = get_station(Key, Monitor),
          Reading = #reading{type = Type, value = Value, date = Date},
          Reading_flag = is_reading_present(Station, Reading, Monitor),
          case Reading_flag of
            true -> {error, reading_already_added};
            _ -> maps:update(Station, lists:append(maps:get(Station, Monitor),[Reading]), Monitor)
          end
  end.

remove_value(Key, Date, Type, Monitor) ->
  Station_flag = is_present_station(Key, Key, Monitor),
  case Station_flag of
    false -> {error, station_does_not_exist};
    _ -> Station = get_station(Key, Monitor),
        Reading_flag = is_reading_present(Station, #reading{type = Type, date = Date}, Monitor),
        case Reading_flag of
          false -> {error, reading_does_not_exist};
          _ -> maps:update(Station, lists:filter(fun(X) -> X#reading.date /= Date orelse X#reading.type /= Type end, maps:get(Station, Monitor)), Monitor)
        end
  end.

get_one_value(Key, Date, Type, Monitor) ->
  Station_flag = is_present_station(Key, Key, Monitor),
  case Station_flag of
    false -> {error, station_does_not_exist};
    _ -> Station = get_station(Key, Monitor),
         Reading_flag = is_reading_present(Station, #reading{type = Type, date = Date}, Monitor),
         case Reading_flag of
           false -> {error, reading_does_not_exist};
           _ ->[#reading{value = Value}|_] = lists:filter(fun(X) -> X#reading.date == Date andalso X#reading.type == Type end, maps:get(Station, Monitor)),
             Value
         end
  end.

get_station_mean(Key, Type, Monitor) ->
  Station_flag = is_present_station(Key, Key, Monitor),
  case Station_flag of
    false -> {error, station_does_not_exist};
    _ -> Station = get_station(Key, Monitor),
          Readings = lists:filter(fun (X) -> X#reading.type == Type end, maps:get(Station, Monitor)),
          case lists:flatlength(Readings) of
            0 -> {error, no_reading_this_type};
            _ -> Mean = lists:foldl(fun (X, Acc) -> X#reading.value + Acc end, 0, Readings)/lists:flatlength(Readings),
                 Mean
          end
end.

get_daily_mean(Type, Date, Monitor) ->
  Readings = lists:filter(fun(X) -> X#reading.type == Type andalso calendar:date_to_gregorian_days(X#reading.date) == calendar:date_to_gregorian_days(Date) end, lists:concat(maps:values(Monitor))),
  case lists:flatlength(Readings) of
    0 -> {error, no_readings_this_type};
    _ -> Mean = lists:foldl(fun (X, Acc) -> X#reading.value + Acc end, 0, Readings)/lists:flatlength(Readings),
        Mean
  end.

get_closest_stations(Monitor) ->
  Dist = fun({X1,Y1},{X2,Y2}) -> math:sqrt(math:pow(X1-X2,2)+math:pow(Y1-Y2,2)) end,
  Stations = [{X,Y} || X <- maps:keys(Monitor), Y <- maps:keys(Monitor), X < Y],
  {_,S} = lists:min(lists:map(fun(X,Y) -> {Dist(X#station.coordinates, Y#station.coordinates),{X,Y}} end,Stations)),
  S.