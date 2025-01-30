-module(mtran).
-export([read_file/0, hello/0, init_tables/0, print_tables/0, process_file/1]).

% Список ключевых слов C#
cs_keywords() -> 
    ["abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", 
     "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", 
     "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", 
     "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", 
     "null", "object", "operator", "out", "override", "params", "private", "protected", "public", 
     "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", 
     "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", 
     "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"].

% Список логических и математических операторов
operators() -> 
    ["+", "-", "*", "/", "%", "&&", "||", "!", "==", "!=", ">", "<", ">=", "<="].

% Инициализация таблиц имен и операторов
init_tables() -> 
    case ets:info(names_table) of
        undefined -> ets:new(names_table, [named_table, set, public]);
        _ -> ets:delete_all_objects(names_table)
    end,
    case ets:info(operators_table) of
        undefined -> ets:new(operators_table, [named_table, set, public]);
        _ -> ets:delete_all_objects(operators_table)
    end,
    io:format("Tables initialized and cleared.~n").

% Обработка файла
process_file(File) -> 
    case file:read_file(File) of
        {ok, Content} -> 
            String = erlang:binary_to_list(Content),
            Tokens = string:tokens(String, " \t\n\r.,;(){}"),
            process_tokens(Tokens),
            print_tables();
        {error, Reason} -> 
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

% Обработка токенов
process_tokens([]) -> ok;
process_tokens([Token | Rest]) -> 
    case lists:member(Token, cs_keywords()) of
        true -> insert_token(names_table, Token, "Keyword");
        false -> 
            case lists:member(Token, operators()) of
                true -> insert_token(operators_table, Token, "Operator");
                false -> ok
            end
    end,
    process_tokens(Rest).

% Вставка токена в таблицу
insert_token(Table, Token, Type) -> 
    case ets:lookup(Table, Token) of
        [] -> 
            Index = length(ets:tab2list(Table)) + 1,
            ets:insert(Table, {Token, Index, Type}),
            io:format("Added to ~s table: {~s, ~p, ~s}~n", [atom_to_list(Table), Token, Index, Type]);
        _ -> ok
    end.

% Вывод содержимого таблиц
print_tables() -> 
    io:format("===== Names Table =====~n"),
    lists:foreach(fun({Token, Index, Type}) -> 
                        io:format("Index: ~p, Token: ~s, Type: ~s~n", [Index, Token, Type])
                  end, ets:tab2list(names_table)),
    io:format("===== Operators Table =====~n"),
    lists:foreach(fun({Token, Index, Type}) -> 
                        io:format("Index: ~p, Token: ~s, Type: ~s~n", [Index, Token, Type])
                  end, ets:tab2list(operators_table)).

% Чтение файла
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

% Приветствие
hello() -> 
    io:format("Hello, Erlang!~n").
