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
parse_statement(["do" | Rest]) ->
    parse_dowhile(["do" | Rest]);
parse_statement(["while" | Rest]) ->
    parse_while(["while" | Rest]);
parse_statement(["foreach" | Rest]) ->
    parse_foreach(["foreach" | Rest]);
parse_statement(["if" | Rest]) ->
    parse_if(["if" | Rest]);
parse_statement(["switch" | Rest]) ->
    parse_switch(["switch" | Rest]);
parse_statement(["using" | Rest]) ->
    {Namespace, Rest1} = parse_namespace(Rest),
    {node(namespace, "", [Namespace]), Rest1};



parse_statement([Token | Rest]) ->
    case Token of
        "using" ->
            {Namespace, [";" | Rest1]} = parse_namespace(Rest),
            {node(using, "using", [Namespace]), Rest1};
        _ ->
            case is_function_call(Token) of
                true ->
                    {Call, Rest1} = parse_function_call(Token, Rest),
                    {Call, Rest1};
                false ->
                    parse_assignment([Token | Rest])
            end
    end.

parse_namespace(Tokens) ->
    {Namespace, Rest} = parse_identifier(Tokens, []),
    {Namespace, Rest}.


parse_identifier([], Acc) -> {lists:reverse(Acc), []};
parse_identifier([";" | Rest], Acc) -> {lists:reverse(Acc), Rest};
parse_identifier([Token | Rest], Acc) ->
    parse_identifier(Rest, [Token | Acc]).

parse_for(["for", "(" | Rest]) ->
    {Init, [";" | Rest1]} = parse_statement(Rest),  
    {Cond, [";" | Rest2]} = parse_expr(Rest1),      
    {Incr, [")" | Rest3]} = parse_increment(Rest2),

    {Body, Rest4} = parse_block(Rest3),            
    {node(for, "for", [Init, Cond, Incr, Body]), Rest4}.

parse_dowhile(["do" | Rest]) ->
    {Body, ["while", "(" | Rest1]} = parse_block(Rest),
    {Cond, [")" | Rest2]} = parse_expr(Rest1),
    {node(dowhile, "do-while", [Body, Cond]), Rest2};
parse_dowhile(_) ->
    error(invalid_do_while_syntax).
parse_while(["while", "(" | Rest]) ->
    {Cond, [")" | Rest1]} = parse_expr(Rest),
    {Body, Rest2} = parse_block(Rest1),
    {node(while, "while", [Cond, Body]), Rest2};
parse_while(_) ->
    error(invalid_while_syntax).

parse_foreach(["foreach", "(" | Rest]) ->
    {ForeachDecl, Rest1} = parse_foreach_declaration(Rest),
    case Rest1 of
        ["in" | Rest2] ->
            {Collection, [")" | Rest3]} = parse_expr(Rest2),
            {Body, Rest4} = parse_block(Rest3),
            {node(foreach, "foreach", [ForeachDecl, Collection, Body]), Rest4};
        _ ->
            error(invalid_foreach_syntax)
    end;
parse_foreach(_) ->
    error(invalid_foreach_syntax).

parse_foreach_declaration([Type, Id | Rest]) ->
    case is_type(Type) of
        true -> {node(decl, Type, [node(id, Id, [])]), Rest};
        false -> error(invalid_type)
    end.

parse_if(["if", "(" | Rest]) ->
    {Cond, [")" | Rest1]} = parse_expr(Rest),
    {IfBody, Rest2} = parse_block(Rest1),
    case Rest2 of
        ["else", "if", "(" | Rest3] ->  
            {ElseIfCond, [")" | Rest4]} = parse_expr(Rest3),
            {ElseIfBody, Rest5} = parse_block(Rest4),
            {ElseBody, Rest6} = parse_if(["if", "(" | Rest5]),  
            {node('if', "if", [
                node(condition, "condition", [Cond]),
                node(if_body, "if_body", [IfBody]),
                node(else_if, "else_if", [
                    node(condition, "condition", [ElseIfCond]),
                    node(else_if_body, "else_if_body", [ElseIfBody]),
                    ElseBody
                ])
            ]), Rest6};
        ["else" | Rest3] ->  
            {ElseBody, Rest4} = parse_block(Rest3),
            {node('if', "if", [
                node(condition, "condition", [Cond]),
                node(if_body, "if_body", [IfBody]),
                node(else_body, "else_body", [ElseBody])
            ]), Rest4};
        _ ->  
            {node('if', "if", [
                node(condition, "condition", [Cond]),
                node(if_body, "if_body", [IfBody])
            ]), Rest2}
    end;
parse_if(_) ->
    error(invalid_if_syntax).

parse_switch(["switch", "(", Expr, ")", "{" | Rest]) ->
    {Cases, Rest1} = parse_cases(Rest, []),
    {node(switch, "switch", [node(expr, Expr, []), node(cases, "cases", Cases)]), Rest1};
parse_switch(_) ->
    error(invalid_switch_syntax).

parse_cases(["}" | Rest], Acc) -> 
    {lists:reverse(Acc), Rest};
parse_cases(["case" | Rest], Acc) ->
    {RawValue, [":" | Rest1]} = parse_expr(Rest),
    {Body, Rest2} = parse_case_body(Rest1, []),
    parse_cases(Rest2, [node('case', RawValue, Body) | Acc]);


parse_cases(["default", ":" | Rest], Acc) ->
    {Body, Rest1} = parse_case_body(Rest, []),
    parse_cases(Rest1, [node(default, "default", Body) | Acc]);
parse_cases(["default:" | Rest], Acc) -> 
    {Body, Rest1} = parse_case_body(Rest, []),
    parse_cases(Rest1, [node(default, "default", Body) | Acc]);
parse_cases(_, _) ->
    error(invalid_case_syntax).

parse_case_body(["break", ";" | Rest], Acc) -> 
    {lists:reverse(Acc), Rest};
parse_case_body(["}" | _] = Rest, Acc) -> 
    {lists:reverse(Acc), Rest};
parse_case_body([";" | Rest], Acc) ->  
    parse_case_body(Rest, Acc);
parse_case_body(Tokens, Acc) ->
    {Stmt, Rest} = parse_statement(Tokens),
    parse_case_body(Rest, [Stmt | Acc]).

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

print_tree(Node, Indent) when is_list(Node) ->
    io:format("~s~s~n", [spaces(Indent), Node]);
print_tree({Type, Value, Children}, Indent) ->
    FormattedValue =
        case Value of
            {string, S, []} -> S;
            _ -> Value
        end,
    io:format("~s~s: ~s~n", [spaces(Indent), Type, FormattedValue]),
    lists:foreach(fun(Child) -> print_tree(Child, Indent + 4) end, Children).


spaces(N) -> lists:duplicate(N, $ ).

is_delimiter(Char) ->
    not ((Char >= $a andalso Char =< $z) orelse 
         (Char >= $A andalso Char =< $Z) orelse 
         (Char >= $0 andalso Char =< $9) orelse 
         Char == $_).
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

tokenize([$\\, $" | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, Acc, true, [$" | StringAcc]); 

%% обработка остальных символов внутри строки
tokenize([Char | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, Acc, true, [Char | StringAcc]);

tokenize([$; | Rest], Acc, false, Current) ->
    NewAcc = case Current of
        [] -> [";" | Acc];
        _  -> [";", lists:reverse(Current) | Acc]
    end,
    tokenize(Rest, NewAcc, false, []);

tokenize("do" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["do" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["do" | Acc], false, []);
                false -> tokenize("o" ++ Rest, Acc, false, "d")
            end
    end;

tokenize("while" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["while" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["while" | Acc], false, []);
                false -> tokenize("hile" ++ Rest, Acc, false, "w")
            end
    end;

tokenize("foreach" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["foreach" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["foreach" | Acc], false, []);
                false -> tokenize("oreach" ++ Rest, Acc, false, "f")
            end
    end;

tokenize("if" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["if" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["if" | Acc], false, []);
                false -> tokenize("f" ++ Rest, Acc, false, "i")
            end
    end;

tokenize("else" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["else" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["else" | Acc], false, []);
                false -> tokenize("lse" ++ Rest, Acc, false, "e")
            end
    end;

tokenize("default:" ++ Rest, Acc, false, []) ->
    tokenize(Rest, [":", "default" | Acc], false, []);

tokenize("using" ++ Rest, Acc, false, []) ->
    case Rest of
        [] -> 
            tokenize(Rest, ["using" | Acc], false, []);
        [NextChar | _] -> 
            case is_delimiter(NextChar) of
                true -> tokenize(Rest, ["using" | Acc], false, []);
                false -> tokenize("sing" ++ Rest, Acc, false, "u")
            end
    end;

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

    tokenize("switch" ++ Rest, Acc, false, []) ->
        case Rest of
            [] -> tokenize(Rest, ["switch" | Acc], false, []);
            [NextChar | _] ->
                case is_delimiter(NextChar) of
                    true -> tokenize(Rest, ["switch" | Acc], false, []);
                    false -> tokenize("witch" ++ Rest, Acc, false, "s")
                end
        end;

%% обработка обычных символов
tokenize([Char | Rest], Acc, false, Current) -> 
    tokenize(Rest, Acc, false, [Char | Current]).