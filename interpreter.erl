-module(interpreter).
-export([interpret/1, interpret_file/1]).

%% Интерпретация AST
interpret(Tree) ->
    io:format("[INFO] Интерпретация начата~n"),
    Env0 = #{},
    interpret_nodes(Tree, Env0),
    io:format("[INFO] Интерпретация завершена~n").

%% Интерпретация AST из файла
interpret_file(Filename) ->
    case semantic:parse_file(Filename) of
        {ok, Tree} -> interpret(Tree);
        {error, Reason} ->
            io:format("[ERROR] Не удалось прочитать AST из файла: ~p~n", [Reason])
    end.

%% Обход списка узлов
interpret_nodes([], Env) -> Env;
interpret_nodes([{program, _, Nodes} | Rest], Env) ->
    Env1 = interpret_nodes(Nodes, Env),
    interpret_nodes(Rest, Env1);
interpret_nodes([Node | Rest], Env) ->
    Env1 = interpret_node(Node, Env),
    interpret_nodes(Rest, Env1).

interpret_node({decl, char, [{assign, '=', [{id, {id, Id}}, Expr]}]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    %% Value — string длины 1, печатаем как '~s' и вручную ставим одинарные кавычки
    io:format("[INFO] Объявлена переменная ~p со значением '~s'~n", [Id, Value]),
    maps:put(Id, Value, Env1);

%% Обработка одного узла
interpret_node({decl, _Type, [{assign, '=', [{id, {id, Id}}, Expr]}]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Объявлена переменная ~p со значением ~p~n", [Id, Value]),
    maps:put(Id, Value, Env1);
interpret_node({assign, '=', [{id, {id, Id}}, Expr]}, Env) ->
    {Value, Env1} = eval_expr(Expr, Env),
    io:format("[INFO] Присвоение переменной ~p значения ~p~n", [Id, Value]),
    maps:put(Id, Value, Env1);
interpret_node({call, {id, 'Console.WriteLine'}, Arg}, Env) ->
    {Value, _} = eval_expr(Arg, Env),
    io:format("[INFO] Вызов Console.WriteLine: ~s~n", [Value]),
    Env;
interpret_node(_, Env) ->
    Env.

%% Вычисление выражений

%% Числовые литералы
eval_expr({num, {int, N}}, Env)      -> {N, Env};
eval_expr({num, {double, N}}, Env)   -> {N, Env};

%% Строка, булево
eval_expr({string, {string, S}}, Env) -> 
    {S, Env};
eval_expr({bool, {bool, B}}, Env) -> 
    {B, Env};

%% Символьные литералы (оба варианта AST: {char,{char,C}} и {char,C})
eval_expr({char, {char, C}}, Env)    -> normalize_char(C, Env);
eval_expr({char, C}, Env)            -> normalize_char(C, Env);




%% Переменные
eval_expr({id, {id, Id}}, Env) ->
    case maps:get(Id, Env, undefined) of
        undefined ->
            io:format("[ERROR] Не определена переменная ~p~n", [Id]),
            {undefined, Env};
        Val ->
            {Val, Env}
    end;

%% Арифметические выражения
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

%% Вложенные бинарные операторы как списки
eval_expr({op, Op, [L, R]}, Env) ->
    eval_expr({op, Op, L, R}, Env);

%% Неизвестные конструкции
eval_expr(Unknown, Env) ->
    io:format("[WARN] Не удалось вычислить узел: ~p~n", [Unknown]),
    {undefined, Env}.

normalize_char(C, Env) ->
    %% C может быть atom или list; делаем atom_to_list или оставляем list
    Base = case C of
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        L    when is_list(L)    -> L
    end,
    %% убираем одиночные кавычки по краям, если они есть
    Stripped = string:trim(Base, both, "'"),
    %% теперь Stripped — список длины 1, возвращаем его
    {Stripped, Env}.
