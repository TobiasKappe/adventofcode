#!/usr/bin/env escript

-mode(compile).


fuel(Mass) ->
    trunc(Mass / 3) - 2.


read_inputs() ->
    case io:fread("", "~u") of
        eof -> [];
        {ok, [Number]} -> [Number|read_inputs()]
    end.


main(_Args) ->
    Inputs = read_inputs(),

    Total = lists:sum(lists:map(fun fuel/1, Inputs)),

    io:format("~B~n", [Total]).
