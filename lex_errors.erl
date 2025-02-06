-module(lex_errors).
-export([check_file/1]).

check_file(FilePath) ->
    io:format("Starting file check: ~ts~n", [FilePath]),
    case file:read_file(FilePath) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),
            check_lines(Lines, 1, false, false);
        {error, Reason} ->
            io:format("File read error ~ts: ~s~n", [FilePath, atom_to_list(Reason)])
    end.

check_lines([], _, _, _) ->
    io:format("File check complete.~n"),
    ok;
check_lines([Line | Rest], LineNum, InString, HasError) ->
    {NewInside, Error} = analyze_line(Line, InString),
    if Error =:= true ->
           io:format("Lexical error: missing closing quote before semicolon at line ~p~n", [LineNum]),
           check_lines(Rest, LineNum + 1, false, true);
       NewInside =:= true ->
           io:format("Lexical error: string literal not closed at line ~p~n", [LineNum]),
           check_lines(Rest, LineNum + 1, false, true);
       true ->
           check_lines(Rest, LineNum + 1, false, HasError)
    end.

analyze_line(Line, InString) ->
    lists:foldl(fun(C, {Inside, Error}) ->
                      case C of
                          $" ->
                              {not Inside, Error};  
                          $; when Inside ->
                              {Inside, true};       
                          _ ->
                              {Inside, Error}
                      end
              end, {InString, false}, Line).
