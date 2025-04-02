-module(test).
-export([run/0]).

run() ->
    case semantic:analyze_file("input.ast") of
        ok -> 
            io:format("OK~n");
        {error, Errors} ->
            io:format("Errors:~n"),
            [io:format(" - ~s~n", [E]) || E <- Errors]
    end.