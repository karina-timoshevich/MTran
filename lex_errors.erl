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
           io:format("  -> Lexical error: missing closing quote before semicolon at line ~p~n", [LineNum]),
           check_lines(Rest, LineNum + 1, false, true);
       NewInside =:= true ->
           io:format("  -> Lexical error: string literal not closed at line ~p~n", [LineNum]),
           check_lines(Rest, LineNum + 1, false, true);
       true ->
           check_for_invalid_patterns(Line, LineNum),
           check_for_increment_decrement_patterns(Line, LineNum),
           check_for_invalid_identifier(Line, LineNum),
           check_for_invalid_logical_operators(Line, LineNum),
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

check_for_invalid_patterns(Line, LineNum) ->
    case re:run(Line, "([+]{3,}|[\\-]{3,}|[+\\-]{3}[=]{1,})") of
        {match, _} ->
            io:format("  -> Lexical error: invalid pattern at line ~p: ~s~n", [LineNum, Line]);
        nomatch ->
            ok
    end.

check_for_increment_decrement_patterns(Line, LineNum) ->
    case re:run(Line, "\\w+\\+{3,}|\\w+\\-{3,}|\\w+\\+\\-{2,}") of
        {match, _} ->
            io:format("  -> Lexical error: invalid increment/decrement pattern at line ~p: ~s~n", [LineNum, Line]);
        nomatch ->
            ok
    end.

check_for_invalid_identifier(Line, LineNum) ->
    SemicolonResult = re:run(Line, ";"),
    SemicolonPresent = case SemicolonResult of
                           nomatch -> false;
                           _ -> true
                       end,
    TypeKeywordResult = re:run(Line, "\\b(?:int|float|string|char|double|long|short|boolean|class)\\b"),
    TypeKeywordPresent = case TypeKeywordResult of
                             nomatch -> false;
                             _ -> true
                         end,
    if 
         SemicolonPresent andalso TypeKeywordPresent ->
            case re:run(Line, "\\b(?:int|float|string|char|double|long|short|boolean|class)\\s+(\\S+)", [{capture, [1], list}]) of
                {match, [Identifier]} ->
                    case re:run(Identifier, "^[a-zA-Z_][a-zA-Z0-9_]*$", [{capture, none}]) of
                        nomatch ->
                            io:format("  -> Lexical error: invalid identifier after type keyword at line ~p: ~s~n", [LineNum, Line]),
                            ok;
                        {match, _} ->
                            ok;
                        _Other ->
                            ok
                    end;
                {match, _Other} ->
                    ok;
                nomatch ->
                    ok;
                _Other ->
                    ok
            end;
         true ->
            ok
    end.

check_for_invalid_logical_operators(Line, LineNum) ->
    Tokens = re:split(Line, "\\s+", [{return, list}, trim]),
    lists:foreach(fun(Token) ->
                          case Token of
                              [C1, C2 | _Rest] ->
                                  case C1 of
                                      $& when C2 =/= $& ->
                                          io:format("  -> Lexical error: invalid logical operator at line ~p: ~s~n", [LineNum, Line]);
                                      $| when C2 =/= $| ->
                                          io:format("  -> Lexical error: invalid logical operator at line ~p: ~s~n", [LineNum, Line]);
                                      _ ->
                                          ok
                                  end;
                              _ ->
                                  ok
                          end
                  end, Tokens),
    ok.