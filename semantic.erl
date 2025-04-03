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
                    case string:chr(VTrim, $.) of
                        0 -> {int, list_to_integer(VTrim)};
                        _ -> {double, list_to_float(VTrim)}
                    end;
                string -> {string, string:trim(VTrim, both, "\"")};
                id -> {id, list_to_atom(VTrim)};
                bool -> {bool, list_to_atom(VTrim)};
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
                [{id, Id}, ExprNode] ->
                    NewContext = add_variable(Id, Type, Context),
                    check_assignment(Type, Id, ExprNode, NewContext, Errors);
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
    case check_expression(Left, Context) of
        {error, Msg} -> {ok, Context, [Msg | Errors]};
        {ok, LeftType, Ctx1} ->
            case check_expression(Right, Ctx1) of 
                {error, Msg} -> {ok, Context, [Msg | Errors]};
                {ok, RightType, Ctx2} ->
                    case check_operator(Op, Left, Right, LeftType, RightType, Ctx2) of
                        {ok, _} -> {ok, Ctx2, Errors};
                        {error, Msg} -> {ok, Ctx2, [Msg | Errors]}
                    end
            end
    end;
check_node({'', _, Nodes}, Context, Errors) ->
    check_children(Nodes, Context, Errors);
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
            UpdatedContext = add_variable(Id, Type, NewContext),
            io:format("[DEBUG] Declared ~p as ~p. Value type: ~p~n", [Id, Type, ExprType]),
            case is_convertible(ExprType, Type) of
                true -> {ok, UpdatedContext, Errors};
                false ->
                    Msg = format_error("Type mismatch: ~p (declared as ~p) got ~p", [Id, Type, ExprType]),
                    {ok, UpdatedContext, [Msg | Errors]}
            end;
        {error, Msg} -> {ok, Context, [Msg | Errors]}
    end.


check_expression({num, {int, _}}, Context) -> 
    {ok, int, Context};
check_expression({num, {double, _}}, Context) -> 
    {ok, double, Context};

check_expression({string, _}, Context) ->
    {ok, string, Context};
check_expression({bool, _}, Context) ->
    {ok, bool, Context};
check_expression({char, _}, Context) ->
    {ok, char, Context};

check_expression({var, Value}, Context) ->
    case check_expression(Value, Context) of
        {ok, Type, _} -> {ok, Type, Context};
        {error, Msg} -> {error, Msg}
    end;


check_expression({id, Id}, Context) ->
    case find_variable(Id, Context) of
        {ok, Type} -> {ok, Type, Context};
        not_found -> {error, format_error("Use of undeclared variable: ~p", [Id])}
    end;

check_expression({op, Op, Left, Right}, Context) ->
    case check_expression(Left, Context) of
        {error, Msg} -> {error, Msg};
        {ok, LeftType, Ctx1} ->
            case check_expression(Right, Ctx1) of
                {error, Msg} -> {error, Msg};
                {ok, RightType, Ctx2} ->
                    case check_operator(Op, Left, Right, LeftType, RightType, Ctx2) of
                        {ok, Type} -> {ok, Type, Ctx2};
                        {error, Msg} -> {error, Msg}
                    end
            end
    end;


check_expression({op, Op, Args}, Context) when is_list(Args) ->
    case Args of
        [Left, Right] -> check_expression({op, Op, Left, Right}, Context);
        _ -> {error, "Invalid operator structure"}
    end.
check_operator('+', LeftNode, RightNode, LeftType, RightType, Context) ->
    LeftStr = expression_to_string(LeftNode, Context), 
    RightStr = expression_to_string(RightNode, Context),
    case {LeftType, RightType} of
        {int, int} -> {ok, int};
        {double, double} -> {ok, double};
        {int, double} -> {ok, double};
        {double, int} -> {ok, double};
        {string, string} -> {ok, string};
        _ ->
            {error, format_error("Invalid operation: ~s + ~s. Left: ~s, Right: ~s",
                                [type_to_str(LeftType), type_to_str(RightType), LeftStr, RightStr])}
    end;

check_operator(Op, LeftNode, RightNode, LeftType, RightType, Context) ->
    LeftStr = expression_to_string(LeftNode, Context),
    RightStr = expression_to_string(RightNode, Context),
    case Op of
        '+' ->
            case {LeftType, RightType} of
                {int, int} -> {ok, int};
                {double, double} -> {ok, double};
                {int, double} -> {ok, double};
                {double, int} -> {ok, double};
                {string, string} -> {ok, string};
                _ ->
                 
                    {error, format_error("Invalid operation: ~s + ~s. Left: ~s, Right: ~s",
                                [type_to_str(LeftType), type_to_str(RightType), LeftStr, RightStr])}
            end;
        '-' ->
            case {LeftType, RightType} of
                {int, int} -> {ok, int};
                {double, double} -> {ok, double};
                {int, double} -> {ok, double};
                {double, int} -> {ok, double};
                _ ->
                    {error, format_error("Invalid operation: ~s - ~s. Left operand: ~s, Right operand: ~s",
                                        [type_to_str(LeftType), type_to_str(RightType), LeftStr, RightStr])}
            end;
        '*' ->
            case {LeftType, RightType} of
                {int, int} -> {ok, int};
                {double, double} -> {ok, double};
                {int, double} -> {ok, double};
                {double, int} -> {ok, double};
                {string, int} -> {ok, string};
                {int, string} -> {ok, string};
                _ ->
                    {error, format_error("Invalid operation: ~s * ~s. Left operand: ~s, Right operand: ~s",
                                        [type_to_str(LeftType), type_to_str(RightType), LeftStr, RightStr])}
            end;
        _ ->
            {error, format_error("Unsupported operator: ~s", [Op])}
    end.

type_to_str(T) ->
    case T of
        int -> "int";
        double -> "double";
        string -> "string";
        bool -> "bool";
        char -> "char";
        var -> "var";
        _ -> "unknown"
    end.

expression_to_string({id, {id, Name}}, _Context) ->
    atom_to_list(Name);
expression_to_string({num, {int, N}}, _) -> integer_to_list(N);
expression_to_string({num, {double, N}}, _) -> float_to_list(N);
expression_to_string({string, {string, S}}, _) -> "\"" ++ S ++ "\"";
expression_to_string({bool, {bool, B}}, _) -> atom_to_list(B);
expression_to_string({char, {char, C}}, _) -> [C];
expression_to_string(_, _) -> "unknown".

add_variable(Id, Type, [{'Scope', Vars, C} | Rest]) ->
    [{'Scope', [{Id, Type} | Vars], C} | Rest].

find_variable(Id, [{'Scope', Vars, _} | Rest]) ->
    case proplists:get_value(Id, Vars) of
        undefined -> find_variable(Id, Rest);
        Type -> {ok, Type}
    end;
find_variable(_, []) -> not_found.

is_convertible(From, To) ->
    case {From, To} of
        {int, int} -> true;
        {int, double} -> true; 
        {double, double} -> true;
        {string, string} -> true;
        {bool, bool} -> true;
        {char, char} -> true;
        {var, _} -> true;      
        {_, var} -> true;     
        _ -> false
    end.

format_error(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
