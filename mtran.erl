-module(mtran).
-export([read_file/0, hello/0, init_table/0, process_file/1]).

% List of C# keywords
cs_keywords() ->
    ["abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"].

% Initialize keyword table
init_table() ->
    case ets:info(keyword_table) of
        undefined ->
            ets:new(keyword_table, [named_table, set, public]),
            io:format("Keyword table initialized.~n");
        _ ->
            io:format("Keyword table already exists.~n")
    end.

% Process file function
process_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            String = erlang:binary_to_list(Content),
            Tokens = string:tokens(String, " \t\n\r.,;(){}"),
            process_tokens(Tokens, 1);
        {error, Reason} ->
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

% Token processing function
process_tokens([], _) -> ok;
process_tokens([Token | Rest], Index) ->
    case lists:member(Token, cs_keywords()) of
        true -> % If the token is a C# keyword
            case ets:lookup(keyword_table, Token) of
                [] -> % If token does not exist, add it
                    ets:insert(keyword_table, {Index, Token, "Keyword"}),
                    io:format("Added to table: {~p, ~s, \"Keyword\"}~n", [Index, Token]),
                    process_tokens(Rest, Index + 1);
                [{ExistingIndex, Token, _}] -> % If token already exists, reuse index
                    io:format("Reused: {~p, ~s}~n", [ExistingIndex, Token]),
                    process_tokens(Rest, Index)
            end;
        false -> % Ignore non-keywords
            process_tokens(Rest, Index)
    end.

% File reading function
read_file() ->
    File = "D:/6_SEM/МТран/input.txt", % File path
    case file:read_file(File) of
        {ok, Content} ->
            io:format("Content type: ~p~n", [Content]),
            String = erlang:binary_to_list(Content),
            io:format("File content: ~s~n", [String]);
        {error, Reason} ->
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

% Greeting function
hello() ->
    io:format("Hello, Erlang!~n").
