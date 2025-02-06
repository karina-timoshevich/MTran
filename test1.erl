-module(test1).
-export([parse_strings_from_file/1, parse_chars_from_file/1]).

parse_strings_from_file(FilePath) ->
    {ok, Code} = file:read_file(FilePath),
    Lines = string:split(erlang:binary_to_list(Code), "\n", all),
    Strs = lists:foldl(fun(Line, Acc) ->
                          case match_string_assignment(Line) of
                              {ok, Str} -> [Str | Acc];
                              error -> Acc
                          end
                      end, [], Lines),
    lists:reverse(Strs).

match_string_assignment(Line) ->
    Pattern = "^[ \\t]*string[ \\t]+\\w+[ \\t]*=[ \\t]*\"([^\"]*)\"",
    case re:run(Line, Pattern, [{capture, [1], list}]) of
        {match, [Str]} -> {ok, Str};
        nomatch -> error
    end.

parse_chars_from_file(FilePath) ->
    {ok, Code} = file:read_file(FilePath),
    Lines = string:split(erlang:binary_to_list(Code), "\n", all),
    Chars = lists:foldl(fun(Line, Acc) ->
                          case match_char_assignment(Line) of
                              {ok, Char} -> [Char | Acc];
                              error -> Acc
                          end
                      end, [], Lines),
    lists:reverse(Chars).

match_char_assignment(Line) ->
    Pattern = "^[ \\t]*char[ \\t]+\\w+[ \\t]*=[ \\t]*'([^']+)'",
    case re:run(Line, Pattern, [{capture, [1], list}]) of
        {match, [Char]} -> {ok, Char};
        nomatch -> error
    end.
