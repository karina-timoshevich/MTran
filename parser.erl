-module(parser).
-export([parse/1, print_tree/1, parse_file/1]).

node(Type, Value, Children) -> {Type, Value, Children}.

parse(Tokens) ->
    {AST, []} = parse_statements(Tokens, []),
    AST.

parse_statements([], Acc) -> {node(program, "root", lists:reverse(Acc)), []};
parse_statements(Tokens, Acc) ->
    {Stmt, Rest} = parse_statement(Tokens),
    parse_statements(Rest, [Stmt | Acc]).

parse_statement([Type, Id, "=" | Rest]) ->
    case is_type(Type) of
        true ->
            {Expr, Rest1} = parse_expr(Rest),
            {node(decl, Type, [node(assign, "=", [node(id, Id, []), Expr])]), Rest1};
        false ->
            parse_assignment([Type, Id, "=" | Rest])
    end;
parse_statement(Tokens) ->
    parse_assignment(Tokens).

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

tokenize([$" | Rest], Acc, false, []) -> 
    tokenize(Rest, Acc, true, [$"]);

tokenize([$" | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, [lists:reverse([$" | StringAcc]) | Acc], false, []); 

tokenize([Char | Rest], Acc, true, StringAcc) -> 
    tokenize(Rest, Acc, true, [Char | StringAcc]); 

tokenize([$; | Rest], Acc, false, []) -> 
    tokenize(Rest, Acc, false, []); 
tokenize([$; | Rest], Acc, false, Current) -> 
    tokenize(Rest, [lists:reverse(Current) | Acc], false, []);

tokenize([$\r | Rest], Acc, false, []) ->
    tokenize(Rest, Acc, false, []);
tokenize([$\r | Rest], Acc, false, Current) ->
    tokenize(Rest, [lists:reverse(Current) | Acc], false, []);
tokenize([$\n | Rest], Acc, false, []) ->
    tokenize(Rest, Acc, false, []);
tokenize([$\n | Rest], Acc, false, Current) ->
    tokenize(Rest, [lists:reverse(Current) | Acc], false, []);
tokenize([Char | Rest], Acc, false, Current) when Char =:= 32; Char =:= 9 -> 
    case Current of
        [] -> tokenize(Rest, Acc, false, []);
        _  -> tokenize(Rest, [lists:reverse(Current) | Acc], false, [])
    end;

tokenize([Char | Rest], Acc, false, Current) -> 
    tokenize(Rest, Acc, false, [Char | Current]).