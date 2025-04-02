-module(semantic).
-export([analyze/1, analyze_file/1]).

analyze(Tree) ->
    InitialContext = [{'Scope', [], []}],
    case check_children(Tree, InitialContext, []) of
        {ok, _, Errors} when Errors =/= [] ->
            {error, lists:reverse(Errors)};
        {ok, _, _} ->
            ok;
        {error, Errors} ->
            {error, lists:reverse(Errors)}
    end.

analyze_file(Filename) ->
    case parse_file(Filename) of
        {ok, Tree} ->
            io:format("[DEBUG] AST: ~p~n", [Tree]),
            analyze(Tree);
        {error, Reason} -> {error, Reason}
    end.

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = string:split(unicode:characters_to_list(Data), "\n", all),
    {Nodes, _Remaining} = parse_block(Lines, 0),
    {ok, Nodes}.

parse_block([], _CurrentIndent) ->
    {[], []};
parse_block([Line | Rest], CurrentIndent) ->
    {Indent, Node} = parse_line(Line),
    if
        Indent < CurrentIndent ->
            {[], [Line | Rest]};
        Indent == CurrentIndent ->
            {Children, Rem1} = parse_block(Rest, CurrentIndent + 1),
            Node1 = case Children of
                        [] -> Node;
                        _  -> add_children(Node, Children)
                    end,
            {Siblings, Rem2} = parse_block(Rem1, CurrentIndent),
            {[Node1 | Siblings], Rem2};
        Indent > CurrentIndent ->
            {[], [Line | Rest]}
    end.

add_children({Type, Value}, Children) ->
    {Type, Value, Children}.

parse_line(Line) ->
    LineTrim = string:trim(Line),
    IndentLevel = (string:length(Line) - string:length(LineTrim)) div 4,
    [TypePart | ValueParts] = string:split(LineTrim, ":", leading),
    Type = list_to_atom(string:trim(TypePart)),
    Value = case ValueParts of
        [] -> [];
        [V] ->
            VTrim = string:trim(V),
            case Type of
                num -> 
                    case catch list_to_float(VTrim) of
                        {'EXIT', _} -> {error, "Invalid number"};
                        Float -> {num, Float}
                    end;
                string -> {string, string:trim(VTrim, both, "\"")};
                char -> {char, string:trim(VTrim, both, "'")};  
                id -> {id, list_to_atom(VTrim)};
                bool -> {bool, list_to_atom(VTrim)};
                var -> {var, VTrim};  %% Просто сохраняем строковое представление
                _ -> list_to_atom(VTrim)
            end
    end,
    {IndentLevel, {Type, Value}}.



check_node({program, _, Nodes}, Context, Errors) ->
    check_children(Nodes, Context, Errors);

check_node({decl, Type, Children}, Context, Errors) ->
    case lists:keyfind(assign, 1, Children) of
        {assign, '=', AssignChildren} ->
            case AssignChildren of
                [IdNode, ExprNode] ->
                    {id, Id} = IdNode,
                    check_assignment(Type, Id, ExprNode, Context, Errors);
                _ ->
                    {ok, Context, [format_error("Invalid assignment structure", []) | Errors]}
            end;
        _ ->
            {ok, Context, [format_error("Invalid declaration: ~s", [Type]) | Errors]}
    end;

check_node({assign, '=', Children}, Context, Errors) ->
    case Children of
        [IdNode, ExprNode] ->
            {id, _} = IdNode,
            case check_expression(ExprNode, Context) of
                {ok, _, NewContext} ->
                    {ok, NewContext, Errors};
                {error, Msg} ->
                    {ok, Context, [Msg | Errors]}
            end;
        _ ->
            {ok, Context, [format_error("Invalid assignment structure", []) | Errors]}
    end;

check_node({op, Op, Left, Right}, Context, Errors) ->
    case {check_expression(Left, Context), check_expression(Right, Context)} of
        {{ok, LeftType, _}, {ok, RightType, _}} ->
            case check_operator(Op, LeftType, RightType) of
                {ok, _} -> {ok, Context, Errors};
                {error, _} ->
                    NewErrors = [format_error("Invalid operation: ~s ~s ~s", [LeftType, Op, RightType]) | Errors],
                    {ok, Context, NewErrors}
            end;
        _ -> {ok, Context, Errors}
    end;

check_node(_, Context, Errors) ->
    {ok, Context, Errors}.

check_children([], Context, Errors) ->
    {ok, Context, Errors};
check_children([Node | Rest], Context, Errors) ->
    case check_node(Node, Context, Errors) of
        {ok, NewCtx, NewErrors} ->
            check_children(Rest, NewCtx, NewErrors);
        {error, NewErrors} ->
            {error, NewErrors}
    end.

check_assignment(Type, Id, Expr, Context, Errors) ->
    case check_expression(Expr, Context) of
        {ok, ExprType, NewContext} ->
            io:format("[DEBUG] Assigning ~p (~p) to ~p (~p)~n",
                      [Id, ExprType, Id, Type]),
            case is_convertible(ExprType, Type) of
                true ->
                    UpdatedContext = add_variable(Id, Type, NewContext),
                    {ok, UpdatedContext, Errors};
                false ->
                    NewErrors = [format_error("Type mismatch: variable ~p declared as ~p cannot be assigned a value of type ~p", 
                                               [Id, Type, ExprType])
                                 | Errors],
                    {ok, NewContext, NewErrors}
            end;
        {error, Msg} ->
            {ok, Context, [Msg | Errors]}
    end.


check_expression({num, _}, Context) ->
    {ok, number, Context};
check_expression({string, _}, Context) ->
    {ok, string, Context};
check_expression({bool, _}, Context) ->
    {ok, bool, Context};
check_expression({char, _}, Context) ->
    {ok, char, Context};

check_expression({var, Value}, Context) ->
    %% "var" может быть любым, сначала определяем его тип по значению
    case check_expression(Value, Context) of
        {ok, Type, _} -> {ok, Type, Context};
        {error, Msg} -> {error, Msg}
    end;


check_expression({id, Id}, Context) ->
    case find_variable(Id, Context) of
        {ok, Type} ->
            {ok, Type, Context};
        not_found ->
            {error, format_error("Undefined variable: ~s", [Id])}
    end;
check_expression({op, Op, L, R}, Context) ->
    case {check_expression(L, Context), check_expression(R, Context)} of
        {{ok, LT, _}, {ok, RT, _}} ->
            case check_operator(Op, LT, RT) of
                {ok, T} -> {ok, T, Context};
                {error, _} -> {error, "Invalid operator types"}
            end;
        _ -> {error, "Invalid expression"}
    end.

check_operator('+', number, number) ->
    {ok, number};
check_operator('*', number, number) ->
    {ok, number};
check_operator(_, _, _) ->
    {error, "Invalid operator"}.

add_variable(Id, Type, [{'Scope', Vars, C} | Rest]) ->
    [{'Scope', [{Id, Type} | Vars], C} | Rest].

find_variable(Id, [{'Scope', Vars, _} | Rest]) ->
    case proplists:get_value(Id, Vars) of
        undefined -> find_variable(Id, Rest);
        Type -> {ok, Type}
    end;
find_variable(_, []) ->
    not_found.

is_convertible(From, To) ->
    case {From, To} of
        {string, string} -> true;
        {number, number} -> true;
        {number, double} -> true;
        {bool, bool} -> true;
        {char, char} -> true;
        {var, _} -> true;  %% var можно приравнивать к любому типу
        {_, var} -> true;  %% и любой тип можно приравнивать к var
        _ -> false
    end.


format_error(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
