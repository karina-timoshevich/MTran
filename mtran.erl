-module(mtran).
-export([read_file/0, hello/0, init_table/0,print_names_table/0, process_file/1]).

% List of C# keywords
cs_keywords() ->
    ["abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"].

% Инициализация таблицы имен с индексацией
init_table() -> 
    case ets:info(names_table) of
        undefined -> 
            ets:new(names_table, [named_table, set, public]),
            io:format("Names table initialized.~n");
        _ -> 
            ets:delete_all_objects(names_table), % Очистка перед повторным заполнением
            io:format("Names table cleared and ready for use.~n")
    end.


% Process file function
process_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            String = erlang:binary_to_list(Content),
            Tokens = string:tokens(String, " \t\n\r.,;(){}"),
            process_tokens(Tokens),
            print_names_table();  % Вывод таблицы имен после обработки
        {error, Reason} ->
            io:format("Error reading file ~s: ~s~n", [File, atom_to_list(Reason)])
    end.

% Token processing function
process_tokens([]) -> ok;
process_tokens([Token | Rest]) -> 
    case lists:member(Token, cs_keywords()) of
        true -> % Если токен - ключевое слово
            case ets:lookup(names_table, Token) of
                [] -> 
                    Index = length(ets:tab2list(names_table)) + 1, % Генерация нового индекса
                    ets:insert(names_table, {Token, Index, "Keyword"}),
                    io:format("Added to table: {~s, ~p, \"Keyword\"}~n", [Token, Index]);
                _ -> ok % Если токен уже есть в таблице, ничего не добавляем
            end;
        false -> ok
    end,
    process_tokens(Rest).

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

% Форматированный вывод таблицы имен
print_names_table() ->
    lists:foreach(fun({Token, Index, Type}) -> 
                        io:format("Index: ~p, Token: ~s, Type: ~s~n", [Index, Token, Type])
                  end, ets:tab2list(names_table)).


% Greeting function
hello() ->
    io:format("Hello, Erlang!~n").
