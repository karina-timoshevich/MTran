-module(parser).
-export([parse/1, print_tree/1]).

node(Type, Value, Children) -> {Type, Value, Children}.

parse(Tokens) ->
    {AST, []} = parse_assignment(Tokens),
    AST.

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
        false -> {node(id, Token, []), Rest}
    end.

is_identifier(Token) ->
    re:run(Token, "^[A-Za-z_][A-Za-z0-9_]*$") =/= nomatch.

is_num(Token) ->
    re:run(Token, "^[0-9]+(\\.[0-9]+)?$") =/= nomatch.

print_tree(Tree) ->
    print_tree(Tree, 0),
    ok. 

print_tree({Type, Value, Children}, Indent) ->
    io:format("~s~s: ~s~n", [spaces(Indent), Type, Value]),
    lists:foreach(fun(Child) -> print_tree(Child, Indent + 4) end, Children).

spaces(N) -> lists:duplicate(N, $ ).