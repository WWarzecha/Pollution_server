%%%-------------------------------------------------------------------
%%% @author HP 8-Gen
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2024 20:40
%%%-------------------------------------------------------------------
-module(readingsProcessing).
-author("HP 8-Gen").

%% API
-export([returnReadings/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

returnReadings() -> [
  ["ulica1",
    {
      {day,19},
      {month, 3},
      {year, 2024}
    },
    [
      {pm10, 15},
      {'pm2.5', 13}
    ]
  ],
  ["ulica2",
    {
      {day,19},
      {month, 3},
      {year, 2024}
    },
    [
      {pm10, 17},
      {'pm2.5', 12}
    ]
  ],
  ["ulica3",
    {
      {day,8},
      {month, 3},
      {year, 2024}
    },
    [
      {pm10, 20},
      {pm1, 1}
    ]
  ]
].

%%number_of_readings(Readings, Date) -> int
number_of_readings([], _) -> 0;
number_of_readings([[_,Date,_]|T], Date) -> 1 + number_of_readings(T,Date);
number_of_readings([_|T], Date) -> number_of_readings(T,Date).

%%calculate_max(Readings, Type) -> float
calculate_max([], _Type) -> -1;
calculate_max([[_,_,Param]|T], Type) -> max(get_measurement(Param, Type), calculate_max(T, Type)).

get_measurement([], _Type) -> 0;
get_measurement([{Type, Value}|_], Type) -> Value;
get_measurement([_|T], Type) -> get_measurement(T, Type).

%%calculate_mean(Readings, Type) -> float
calculate_mean(Readings, Type) -> sumReadings(Readings, Type)/countReadings(Readings).

sumReadings(Readings, Type) -> sumReadings(Readings, Type, 0).
sumReadings([], _, Acc) -> Acc;
sumReadings([[_, _, Param] | T], Type, Acc) -> sumReadings(T, Type, Acc + get_measurement(Param, Type)).

countReadings(Readings) -> countReadings(Readings, 0).
countReadings([], Counter) -> Counter;
countReadings([_|T], Counter) -> countReadings(T, Counter + 1).