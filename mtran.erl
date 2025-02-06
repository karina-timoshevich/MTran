-module(mtran).
-export([read_file/0, hello/0, init_tables/0, print_tables/0, process_file/1]).

cs_keywords() -> 
    ["abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", 
     "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", 
     "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", 
     "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", 
     "null", "object", "operator", "out", "override", "params", "private", "protected", "public", 
     "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", 
     "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", 
     "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"].

operators() -> 
    ["+", "-", "*", "/", "%", "&&", "||", "!", "==", "!=", ">", "<", ">=", "<="].

delimiters() ->
    [";", ",", ".", "(", ")", "{", "}", "[", "]"].

is_constant(Token) -> 
    case re:run(Token, "^[+-]?[0-9]+$", [{capture, none}]) of
        match -> {true, "Integer"};
        nomatch -> case re:run(Token, "^[+-]?[0-9]+(\\.[0-9]+)?$", [{capture, none}]) of  
            match -> {true, "Float"};
            nomatch -> case re:run(Token, "^[+-]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)$", [{capture, none}]) of  
                match -> {true, "Scientific"};
                nomatch -> {false, ""}
            end
        end
    end.

init_tables() -> 
    lists:foreach(fun(Table) ->
        case ets:info(Table) of
            undefined -> ets:new(Table, [named_table, set, public]);
            _ -> ets:delete_all_objects(Table)
        end
    end, [names_table, operators_table, delimiters_table, constants_table]), 
    io:format("Tables initialized and cleared.~n").


process_file(File) -> 
    case file:read_file(File) of
        {ok, Content} -> 
            String = erlang:binary_to_list(Content),
            Tokens = re:split(String, "([ \t\n\r;,(){}\\[\\]])", [{return, list}, trim]),
            process_tokens(Tokens),
            StringConstants = test1:parse_strings_from_file(File),
            lists:foreach(fun(Str) -> insert_token(constants_table, Str, "String") end, StringConstants),
            CharConstants = test1:parse_chars_from_file(File),
            lists:foreach(fun(Char) -> insert_token(constants_table, Char, "Char") end, CharConstants),
            print_tables();
        {error, Reason} -> 
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

process_tokens([]) -> ok;
process_tokens([Token | Rest]) -> 
    case lists:member(Token, cs_keywords()) of
        true -> insert_token(names_table, Token, "Keyword");
        false -> 
            case lists:member(Token, operators()) of
                true -> insert_token(operators_table, Token, "Operator");
                false -> 
                    case lists:member(Token, delimiters()) of
                        true -> insert_token(delimiters_table, Token, "Delimiter");
                        false -> 
                            case is_constant(Token) of
                                {true, Type} -> insert_token(constants_table, Token, Type);
                                {false, _} -> ok
                            end
                    end
            end
    end,
    process_tokens(Rest).

insert_token(Table, Token, Type) -> 
    case ets:lookup(Table, Token) of
        [] -> 
            Index = length(ets:tab2list(Table)) + 1,
            ets:insert(Table, {Token, Index, Type}),
            io:format("Added to ~s table: {~s, ~p, ~s}~n", [atom_to_list(Table), Token, Index, Type]);
        _ -> ok
    end.

print_tables() -> 
    lists:foreach(fun({Table, Title}) ->
        io:format("===== ~s =====~n", [Title]),
        lists:foreach(fun({Token, Index, Type}) -> 
                            io:format("Index: ~p, Token: ~s, Type: ~s~n", [Index, Token, Type])
                      end, ets:tab2list(Table))
    end, [{names_table, "Names Table"}, {operators_table, "Operators Table"}, {delimiters_table, "Delimiters Table"}, {constants_table, "Constants Table"}]). 

read_file() -> 
    File = "D:/6_SEM/МТран/input.txt", 
    case file:read_file(File) of
        {ok, Content} -> 
            io:format("Content type: ~p~n", [Content]),
            String = erlang:binary_to_list(Content),
            io:format("File content: ~s~n", [String]);
        {error, Reason} -> 
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

hello() -> 
    io:format("Hello, Erlang!~n").