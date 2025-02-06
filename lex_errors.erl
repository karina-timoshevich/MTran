-module(lex_errors).
-export([check_file/1]).

check_file(FilePath) ->
    io:format("Starting file check: ~ts~n", [FilePath]),
    case file:read_file(FilePath) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),
            check_lines(Lines, 1, false, 0);
        {error, Reason} ->
            io:format("File read error ~s: ~s~n", [lists:flatten(FilePath), atom_to_list(Reason)])
    end.

check_lines([], _, false, _) ->
    io:format("File check complete. No errors found.~n"),
    ok;
check_lines([], OpenLine, true, _) ->
    io:format("Lexical error: missing closing quote, starting from line ~p~n", [OpenLine]),
    ok;
check_lines([Line | Rest], LineNum, InString, OpenLine) ->
    {NewInString, OpenDetected, CloseDetected} = analyze_line(Line, InString),

    case {InString, OpenDetected, CloseDetected} of
        {false, true, false} ->  % нашли открыую
            io:format("  -> Found string opening at line ~p~n", [LineNum]),
            check_lines(Rest, LineNum + 1, true, LineNum);
        {true, false, true} ->  % нашли закрытую
            io:format("  -> Found string closing at line ~p~n", [LineNum]),
            check_lines(Rest, LineNum + 1, false, 0);
        {false, true, true} ->  % все в одной строке и ок
            io:format("  -> Found complete string in one line ~p~n", [LineNum]),
            check_lines(Rest, LineNum + 1, false, 0);
        {true, true, false} ->  % опа, не закрылась кавычка
            io:format("Lexical error: missing quote, starting from line ~p~n", [OpenLine]),
            ok; 
        {true, false, false} -> 
            io:format("Lexical error: missing quote, starting from line ~p~n", [OpenLine]),
            ok;
        _ ->  
            check_lines(Rest, LineNum + 1, NewInString, OpenLine)
    end.

analyze_line(Line, InString) ->
    lists:foldl(fun(C, {Inside, Open, Close}) ->
                        case C of
                            $" -> 
                                {not Inside, Open orelse not Inside, Close orelse Inside};
                            _ -> 
                                {Inside, Open, Close}
                        end
                end, {InString, false, false}, Line).
