-module(parser).
-export([parse/1, print_tree/1, parse_file/1]).

node(Type, Value, Children) -> {Type, Value, Children}.

parse(Tokens) ->
    {AST, []} = parse_statements(Tokens, []),
    AST.

parse_statements([], Acc) -> {node(program, "root", lists:reverse(Acc)), []};

parse_statements([";" | Rest], Acc) ->  
    parse_statements(Rest, Acc);  % скип лишних ;

parse_statements(Tokens, Acc) ->
    {Stmt, Rest} = parse_statement(Tokens),
    parse_statements(Rest, [Stmt | Acc]).

parse_statements(["}" | Rest]) -> {[], ["}" | Rest]};
parse_statements(Tokens) ->
    {Stmt, Rest1} = parse_statement(Tokens),
    {OtherStatements, Rest2} = parse_statements(Rest1),
    {[Stmt | OtherStatements], Rest2}.


parse_statement([Type, Id, "=" | Rest]) ->
    case is_type(Type) of
        true ->
            {Expr, Rest1} = parse_expr(Rest),
            {node(decl, Type, [node(assign, "=", [node(id, Id, []), Expr])]), Rest1};
        false ->
            parse_assignment([Type, Id, "=" | Rest])
    end;
parse_statement(["for" | Rest]) ->
    parse_for(["for" | Rest]);
parse_statement(["while" | Rest]) ->
    parse_while(["while" | Rest]);

parse_statement([Token | Rest]) ->
    case is_function_call(Token) of
        true ->
            {Call, Rest1} = parse_function_call(Token, Rest),
            {Call, Rest1};
        false ->
            parse_assignment([Token | Rest])
    end.

parse_for(["for", "(" | Rest]) ->
    {Init, [";" | Rest1]} = parse_statement(Rest),  
    {Cond, [";" | Rest2]} = parse_expr(Rest1),      
   {Incr, [")" | Rest3]} = parse_increment(Rest2),

    {Body, Rest4} = parse_block(Rest3),            
    {node(for, "for", [Init, Cond, Incr, Body]), Rest4}.

parse_while(["while", "(" | Rest]) ->
    {Cond, [")" | Rest1]} = parse_expr(Rest),
    {Body, Rest2} = parse_block(Rest1),
    {node(while, "while", [Cond, Body]), Rest2}.

parse_increment([Id, "++" | Rest]) ->
    {node(op, "++", [node(id, Id, [])]), Rest};
parse_increment([Id, "--" | Rest]) ->
    {node(op, "--", [node(id, Id, [])]), Rest};
parse_increment(Tokens) ->
    parse_expr(Tokens).

parse_block(["{" | Rest]) ->  
    {Statements, ["}" | RestAfterBlock]} = parse_statements(Rest),
    {FilteredStatements, _} = lists:partition(fun(X) -> X =/= node(id, ";", []) end, Statements),
    {node(block, "block", FilteredStatements), RestAfterBlock}.



parse_assignment([Id, "=" | Rest]) when is_list(Id) ->
    {Expr, Rest1} = parse_expr(Rest),
    {node(assign, "=", [node(id, Id, []), Expr]), Rest1};
parse_assignment(Tokens) ->
    parse_expr(Tokens).

parse_expr(Tokens) ->
    {Term, Rest} = parse_term(Tokens),
    parse_expr_tail(Term, Rest).

parse_expr_tail(Left, ["+" | Rest]) ->
    {Right, Rest1} = parse_term(Rest),
    parse_expr_tail(node(op, "+", [Left, Right]), Rest1);
parse_expr_tail(Left, ["-" | Rest]) ->
    {Right, Rest1} = parse_term(Rest),
    parse_expr_tail(node(op, "-", [Left, Right]), Rest1);
parse_expr_tail(Left, [Op | Rest]) when Op == "<"; Op == ">"; Op == "<="; Op == ">="; Op == "=="; Op == "!=" ->
    {Right, Rest1} = parse_term(Rest),
    {node(op, Op, [Left, Right]), Rest1};
parse_expr_tail(Left, ["++" | Rest]) ->
    {node(op, "++", [Left]), Rest};

parse_expr_tail(Node, Rest) -> {Node, Rest}.

parse_term(Tokens) ->
    {Factor, Rest} = parse_factor(Tokens),
    parse_term_tail(Factor, Rest).

parse_term_tail(Left, ["*" | Rest]) ->
    {Right, Rest1} = parse_factor(Rest),
    parse_term_tail(node(op, "*", [Left, Right]), Rest1);
parse_term_tail(Left, ["/" | Rest]) ->
    {Right, Rest1} = parse_factor(Rest),
    parse_term_tail(node(op, "/", [Left, Right]), Rest1);
parse_term_tail(Node, Rest) -> {Node, Rest}.

parse_factor(["(" | Rest]) ->
    {Expr, [")" | Rest1]} = parse_expr(Rest),
    {Expr, Rest1};
parse_factor([Token | Rest]) ->
    case is_function_call(Token) of
        true -> parse_function_call(Token, Rest);
        false -> parse_simple_factor(Token, Rest)
    end.

parse_simple_factor(Token, Rest) ->
    case is_num(Token) of
        true -> {node(num, Token, []), Rest};
        false ->
            case is_string(Token) of
                true -> {node(string, Token, []), Rest};
                false ->
                    case is_char(Token) of
                        true -> {node(char, Token, []), Rest};
                        false ->
                            case is_bool(Token) of
                                true -> {node(bool, Token, []), Rest};
                                false -> {node(id, Token, []), Rest}
                            end
                    end
            end
    end.

%% является ли токен вызовом функции или метода
is_function_call(Token) ->
    case string:find(Token, "(") of
        nomatch -> false;
        _ -> true
    end.

parse_function_call(Token, Rest) ->
    [FunctionName, ArgsPart] = string:split(Token, "(", trailing),
    {ParsedArgs, Rest1} = parse_args(ArgsPart ++ Rest), 
    {node(call, FunctionName, ParsedArgs), Rest1}.

%% парсинг аргументов функции
parse_args(Args) ->
    {ArgsList, [")" | Rest]} = lists:splitwith(fun(T) -> T /= ")" end, Args),
    ParsedArgs = lists:map(fun parse_arg/1, ArgsList),
    {ParsedArgs, Rest}.

%% парсинг одного аргумента
parse_arg(Arg) ->
    ArgTrimmed = string:trim(Arg),
    case is_num(ArgTrimmed) of
        true -> node(num, ArgTrimmed, []);
        false ->
            case is_string(ArgTrimmed) of
                true -> node(string, ArgTrimmed, []);
                false ->
                    case is_char(ArgTrimmed) of
                        true -> node(char, ArgTrimmed, []);
                        false ->
                            case is_bool(ArgTrimmed) of
                                true -> node(bool, ArgTrimmed, []);
                                false -> node(id, ArgTrimmed, [])
                            end
                    end
            end
    end.


is_bool(Token) ->
    lists:member(Token, ["true", "false"]).

is_type(Token) ->
    lists:member(Token, ["int", "double", "bool", "char", "string", "var"]).

is_num(Token) ->
    re:run(Token, "^[0-9]+(\\.[0-9]+)?$") =/= nomatch.

is_string(Token) when is_list(Token), Token =/= [] ->
    case re:run(Token, "^\".*\"$", [{capture, all, list}]) of
        {match, _} -> true;
        _ -> false
    end;
is_string(_) -> false.


is_char(Token) when is_list(Token), length(Token) >= 3 ->
    case re:run(Token, "^'.'$", [{capture, all, list}]) of
        {match, _} -> true;
        _ -> false
    end;
is_char(_) -> false.

print_tree(Tree) ->
    print_tree(Tree, 0),
    ok.

print_tree({Type, Value, Children}, Indent) ->
    io:format("~s~s: ~s~n", [spaces(Indent), Type, Value]),
    lists:foreach(fun(Child) -> print_tree(Child, Indent + 4) end, Children).

spaces(N) -> lists:duplicate(N, $ ).

parse_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Content = binary_to_list(Binary),
    Tokens = tokenize(Content),
    io:format("Токены: ~p~n", [Tokens]),

    Tree = parse(Tokens),
    print_tree(Tree).

tokenize(Content) ->  
    Tokens = tokenize(Content, [], false, []),  
    [T || T <- Tokens, T =/= ""].    

tokenize([], Acc, false, []) -> 
    lists:reverse(Acc);
tokenize([], Acc, false, Current) -> 
    lists:reverse([lists:reverse(Current) | Acc]);
tokenize([], Acc, true, StringAcc) -> 
    io:format("Ошибка: Незакрытая строка: ~s~n", [lists:reverse(StringAcc)]),
    lists:reverse(Acc); 

%% лбработка открывающей кавычки
tokenize([$" | Rest], Acc, false, []) -> 
    tokenize(Rest, Acc, true, [$"]);

%% обработка закрывающей кавычки
tokenize([$" | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, [lists:reverse([$" | StringAcc]) | Acc], false, []); 

%% обработка символов внутри строки
tokenize([Char | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, Acc, true, [Char | StringAcc]); 

%% Обработка точки с запятой
tokenize([$; | Rest], Acc, false, Current) ->
    NewAcc = case Current of
        [] -> [";" | Acc];
        _  -> [";", lists:reverse(Current) | Acc]
    end,
    tokenize(Rest, NewAcc, false, []);

%% обработка символов новой строки и возврата каретки
tokenize([$\r | Rest], Acc, false, []) ->
    tokenize(Rest, Acc, false, []);
tokenize([$\r | Rest], Acc, false, Current) ->
    tokenize(Rest, [lists:reverse(Current) | Acc], false, []);
tokenize([$\n | Rest], Acc, false, []) ->
    tokenize(Rest, Acc, false, []);
tokenize([$\n | Rest], Acc, false, Current) ->
    tokenize(Rest, [lists:reverse(Current) | Acc], false, []);

%% обработка пробелов и табуляции
tokenize([Char | Rest], Acc, false, Current) when Char =:= 32; Char =:= 9 -> 
    case Current of
        [] -> tokenize(Rest, Acc, false, []);
        _  -> tokenize(Rest, [lists:reverse(Current) | Acc], false, [])
    end;

%% обработка вызова функции/метода (имя с '(')
tokenize([$( | Rest], Acc, false, Current) ->
    case Current of
        [] -> tokenize(Rest, ["(" | Acc], false, []);
        _  -> 
            FuncName = lists:reverse(Current),
            tokenize(Rest, [FuncName ++ "(" | Acc], false, [])
    end;

%% обработка закрывающей скобки
tokenize([$) | Rest], Acc, false, Current) ->
    case Current of
        [] -> tokenize(Rest, [")" | Acc], false, []);
        _  -> tokenize(Rest, [")" | [lists:reverse(Current) | Acc]], false, [])
    end;
tokenize([$+, $+ | Rest], Acc, false, Current) ->
    NewAcc = case Current of
        [] -> ["++" | Acc];
        _ -> ["++", lists:reverse(Current) | Acc]
    end,
    tokenize(Rest, NewAcc, false, []);
%% обработка обычных символов
tokenize([Char | Rest], Acc, false, Current) -> 
    tokenize(Rest, Acc, false, [Char | Current]).