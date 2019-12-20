#!/usr/bin/env escript

-mode(compile).


fuel(Mass) ->
    trunc(Mass / 3) - 2.


fuel_fix(Mass) ->
    Fuel = fuel(Mass),

    if
        Fuel < 0 -> 0;
        Fuel >= 0 -> Fuel + fuel_fix(Fuel)
    end.


read_inputs() ->
    case io:fread("", "~u") of
        eof -> [];
        {ok, [Number]} -> [Number|read_inputs()]
    end.


main(_Args) ->
    Inputs = read_inputs(),

    Total = lists:sum(lists:map(fun fuel_fix/1, Inputs)),

    io:format("~B~n", [Total]).
