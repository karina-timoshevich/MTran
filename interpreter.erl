-module(interpreter).
-export([interpret/1, interpret_file/1]).

interpret(Tree) ->
    io:format("[INFO] Интерпретация начата~n"),
    Env0 = #{},
    interpret_nodes(Tree, Env0),
    io:format("[INFO] Интерпретация завершена~n").

interpret_file(Filename) ->
    case semantic:parse_file(Filename) of
        {ok, Tree} -> interpret(Tree);
        {error, Reason} ->
            io:format("[ERROR] Не удалось прочитать AST из файла: ~p~n", [Reason])
    end.

interpret_nodes([], Env) -> Env;
interpret_nodes([{program, _, Nodes} | Rest], Env) ->
    Env1 = interpret_nodes(Nodes, Env),
    interpret_nodes(Rest, Env1);
interpret_nodes([Node | Rest], Env) ->
    Env1 = interpret_node(Node, Env),
    interpret_nodes(Rest, Env1).

interpret_node({decl, char, [{assign, '=', [{id, {id, Id}}, Expr]}]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Объявлена переменная ~p со значением '~s'~n", [Id, Value]),
    maps:put(Id, Value, Env1);
interpret_node({decl, string, [{assign, '=', [{id, {id, Id}}, Expr]}]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Объявлена строковая переменная ~p со значением ~s~n", [Id, Value]),
    maps:put(Id, Value, Env1);

interpret_node({decl, _Type, [{assign, '=', [{id, {id, Id}}, Expr]}]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Объявлена переменная ~p со значением ~p~n", [Id, Value]),
    maps:put(Id, Value, Env1);
interpret_node({assign, '=', [{id, {id, Id}}, Expr]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Присвоение переменной ~p значения ~p~n", [Id, Value]),
    maps:put(Id, Value, Env1);
interpret_node({call, 'Console.WriteLine', Children}, Env) when is_list(Children) ->
    Values = lists:map(
      fun(Node) ->
        {V,_Env2} = eval_expr(Node, Env),
        V
      end,
      Children
    ),
    Out = lists:flatten(Values),
    io:format("[INFO] Вызов Console.WriteLine: ~s~n", [Out]),
    Env;
interpret_node({for, _Label, [Init, Cond, Inc, {block, _, Body}]}, Env) ->
    Env1 = interpret_node(Init, Env),
    loop_for(Cond, Inc, Body, Env1);
interpret_node({op, '++', [{id,{id,Id}}]}, Env) ->
    do_increment({op,'++', [{id,{id,Id}}]}, Env);
interpret_node({while, _Label, [CondNode, {block, _, Body}]}, Env) ->
    loop_while(CondNode, Body, Env);
interpret_node({dowhile, _Label, [ {block, _, Body}, CondNode ]}, Env) ->
    loop_dowhile(Body, CondNode, Env);
interpret_node({Tag, _, Children}, Env) when Tag =:= 'if' ->
    Cond = get_node_value(condition, Children),
    ThenBlock = get_node_block(if_body, Children),
    ElseBlock = get_node_block(else_body, Children),
    {CondVal, Env1} = eval_expr_node(Cond, Env),
    case CondVal of
        true ->
            io:format("[INFO] Условие if: ИСТИНА, выполняется if_body~n"),
            interpret_nodes(ThenBlock, Env1);
        false ->
            io:format("[INFO] Условие if: ЛОЖЬ, выполняется else_body~n"),
            interpret_nodes(ElseBlock, Env1)
    end;
interpret_node({switch, _, Ch}, Env) ->
    RawExpr  = get_node_value(expr, Ch),
    ExprAST  = case RawExpr of
                   Atom when is_atom(Atom) -> {id,{id,Atom}};
                   Other                    -> Other
               end,
    {Val, Env1} = eval_expr_node(ExprAST, Env),
    Cases      = get_node_cases(cases, Ch),
    do_switch(Val, Cases, Env1);
interpret_node(_, Env) ->
    Env.

do_switch(_Val, [], Env) ->
    Env;

do_switch(Val, [{ 'case', KeyAST, Actions } | Rest], Env) ->
    Key = case KeyAST of
        {string,{string,S}} -> S;
        Atom when is_atom(Atom) ->
            string:trim(atom_to_list(Atom), both, "\"");
        _ ->
            io:format("[WARN] Нестандартный ключ case: ~p~n", [KeyAST]),
            ""
    end,
    ValFixed =
        case Val of
            [Str] when is_list(Str) -> Str;
            _ -> Val
        end,
    io:format("[DEBUG] Сравниваем Val=~p и Key=~p~n", [ValFixed, Key]),
    if
        ValFixed =:= Key ->
            io:format("[INFO] Ветка switch выбрана: ~s~n", [Key]),
            interpret_nodes(Actions, Env);
        true ->
            do_switch(Val, Rest, Env)
    end;


do_switch(_Val, [{default, _, Actions} | _], Env) ->
    io:format("[INFO] Ветка switch: default~n", []),
    interpret_nodes(Actions, Env).

get_node_value(Key, [{Key, _, [V]}     | _]) -> V;
get_node_value(Key, [{Key,      V}    | _]) -> V;
get_node_value(Key, [_               | R]) -> get_node_value(Key, R).

get_node_block(Key, [{Key, _, [{block, _, Block}]} | _]) -> Block;
get_node_block(Key, [_ | Rest]) -> get_node_block(Key, Rest).


get_node_cases(Key, [{Key, _, Cs} | _]) -> Cs;
get_node_cases(Key, [_            | R]) -> get_node_cases(Key, R).

loop_for(CondNode, IncNode, Body, Env) ->
    {CondVal, Env1} = eval_expr_node(CondNode, Env),
    case CondVal of
        true ->
            Env2 = interpret_nodes(Body, Env1),
            Env3 = do_increment(IncNode, Env2),
            loop_for(CondNode, IncNode, Body, Env3);
        false ->
            Env1
    end.

loop_while(CondNode, Body, Env) ->
    {CondVal, Env1} = eval_expr_node(CondNode, Env),
    case CondVal of
        true ->
            Env2 = interpret_nodes(Body, Env1),
            loop_while(CondNode, Body, Env2);
        false ->
            Env1
    end.

loop_dowhile(Body, CondNode, Env) ->
    Env1 = interpret_nodes(Body, Env),
    {CondVal, Env2} = eval_expr_node(CondNode, Env1),
    case CondVal of
        true  -> loop_dowhile(Body, CondNode, Env2);
        false -> Env2
    end.

eval_expr_node({op, Op, L, R}, Env) ->
    eval_expr({op, Op, L, R}, Env);
eval_expr_node({op, Op, Args}, Env) when is_list(Args) ->
    eval_expr({op, Op, Args}, Env);
eval_expr_node(Node, Env) ->
    eval_expr(Node, Env).

do_increment({op, '++', [{id,{id,Id}}]}, Env) ->
    Cur = maps:get(Id, Env),
    maps:put(Id, Cur + 1, Env);
do_increment(_, Env) ->
     Env.

eval_expr({num, {int, N}}, Env)      -> {N, Env};
eval_expr({num, {double, N}}, Env)   -> {N, Env};

eval_expr({string, {string, S}}, Env) ->
    {unescape(S), Env};

eval_expr({bool, {bool, B}}, Env) -> 
    {B, Env};

eval_expr({char, {char, C}}, Env)    -> normalize_char(C, Env);
eval_expr({char, C}, Env)            -> normalize_char(C, Env);

eval_expr({id, IdAtom}, Env) when is_atom(IdAtom) ->
    eval_expr({id, {id, IdAtom}}, Env);
eval_expr({id, {id, Id}}, Env) ->
    case maps:get(Id, Env, undefined) of
        undefined ->
            io:format("[ERROR] Не определена переменная ~p~n", [Id]),
            {undefined, Env};
        Val ->
            {Val, Env}
    end;

eval_expr({op, '<', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV < RV, Env2};
eval_expr({op, '>', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV > RV, Env2};
eval_expr({op, '<=', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV =< RV, Env2};
eval_expr({op, '>=', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV >= RV, Env2};
eval_expr({op, '==', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV =:= RV, Env2};
eval_expr({op, '!=', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV =/= RV, Env2};
eval_expr({op, '+', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV + RV, Env2};
eval_expr({op, '-', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV - RV, Env2};
eval_expr({op, '*', L, R}, Env) ->
    {LV, Env1} = eval_expr(L, Env),
    {RV, Env2} = eval_expr(R, Env1),
    {LV * RV, Env2};

eval_expr({op, Op, [L, R]}, Env) ->
    eval_expr({op, Op, L, R}, Env);

eval_expr(Unknown, Env) ->
    io:format("[WARN] Не удалось вычислить узел: ~p~n", [Unknown]),
    {undefined, Env}.

normalize_char(C, Env) ->
    Base = case C of
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        L    when is_list(L)    -> L
    end,
    Stripped = string:trim(Base, both, "'"),
    {Stripped, Env}.

unescape(Str) when is_list(Str) ->
    Step1 = string:replace(Str, "\\n", "\n", all),
    Step1.
