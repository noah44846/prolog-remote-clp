:- module(remote_clp, [
    op(760, yfx, #<==>),
    op(750, xfy, #==>),
    op(750, yfx, #<==),
    op(740, yfx, #\/),
    op(730, yfx, #\),
    op(720, yfx, #/\),
    op(710,  fy, #\),
    op(700, xfx, #>),
    op(700, xfx, #<),
    op(700, xfx, #>=),
    op(700, xfx, #=<),
    op(700, xfx, #=),
    op(700, xfx, #\=),
    op(700, xfx, in),
    op(700, xfx, ins),
    op(450, xfx, ..),
    % op(150, fx, #),
    (#<==>)/2,
    (#==>)/2,
    (#<==)/2,
    (#\/)/2,
    (#\)/2,
    (#/\)/2,
    (#\)/1,
    (#/\)/2,
    (#>)/2,
    (#<)/2,
    (#>=)/2,
    (#=<)/2,
    (#=)/2,
    (#\=)/2,
    (in)/2,
    (ins)/2,
    api_config/1,
    fd_var/1,
    labeling/2,
    label/1,
    all_different/1
]).

:- use_module(library(uuid)).
:- use_module(library(settings)).
:- use_module(library(lists)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

:- setting(api_url, atom, 'http://localhost:3000', 'URL of the remote CLP server').

:- initialization(nb_setval(constraints, [])).

% hook called when a variable with an attribute is unified
% attr_unify_hook(M, E) :- 
%     writeln('Variable was unified with:'),
%     write('value: '), writeln(E),
%     write('had rclp attribute: '), writeln(M).
attr_unify_hook(_, _).


% api_config(+Config)
api_config([]).
api_config([url(URL)|Ls]) :-
    set_setting(api_url, URL),
    api_config(Ls).


% valid_domain(+Lb, +Ub)
valid_domain(Lb, Ub) :-
    (integer(Lb) ; Lb = inf),
    (integer(Ub) ; Ub = sup).


% get_remote_clp_attr(+Var, -Uuid, -Ub, -Lb)
get_remote_clp_attr(Var, Uuid, Lb, Ub) :-
    get_attr(Var, remote_clp, var(Uuid, Lb, Ub)),
    uuid_property(Uuid, version(4)).


% put_remote_clp_attr(+Var, +Uuid, +Ub, +Lb)
put_remote_clp_attr(Var, Uuid, Lb, Ub) :-
    put_attr(Var, remote_clp, var(Uuid, Lb, Ub)).


% serialize_vars(+VarsAttr, -VarsDict)
serialize_vars([], []).
serialize_vars([var(Uuid,Lb,Ub)|Ls1], [Json|Ls2]) :-
    uuid_property(Uuid, version(4)),
    JsonAttrs = [id=Uuid],
    (Lb = inf -> JsonAttrs1 = JsonAttrs ; JsonAttrs1 = [lb=Lb|JsonAttrs]),
    (Ub = sup -> JsonAttrs2 = JsonAttrs1 ; JsonAttrs2 = [ub=Ub|JsonAttrs1]),
    Json = json(JsonAttrs2),
    serialize_vars(Ls1, Ls2).


vars_to_vars_attr([], []).
vars_to_vars_attr([Var|Vars], [var(Uuid, Lb, Ub)|VarsAttr]) :-
    get_remote_clp_attr(Var, Uuid, Lb, Ub),
    vars_to_vars_attr(Vars, VarsAttr).

vars_to_uuids([], []).
vars_to_uuids([Var|Vars], [Uuid|Uuids]) :-
    get_remote_clp_attr(Var, Uuid, _, _),
    vars_to_uuids(Vars, Uuids).

% constraints_for_vars(+Vars, -Constraints, -InvolvedVarsAttr)
constraints_for_vars_aux(_, [], [], []).
constraints_for_vars_aux(VarsAttr, Constraints, InvolvedVarsAttr, [ConstraintTerm|AllConstraints]) :-
    ConstraintTerm = constraint(Constraint, InvolvedVarsAttr1),
    intersection(VarsAttr, InvolvedVarsAttr1, SharedVarsAttr),
    (SharedVarsAttr = [] ->
        (Constraints = Constraints1,
        NewInvolvedVarsAttr = []) ;
        (Constraints = [Constraint|Constraints1],
        NewInvolvedVarsAttr = InvolvedVarsAttr1)),
    constraints_for_vars_aux(VarsAttr, Constraints1, InvolvedVarsAttr2, AllConstraints),
    union(VarsAttr, NewInvolvedVarsAttr, InvolvedVarsAttr3),
    union(InvolvedVarsAttr2, InvolvedVarsAttr3, InvolvedVarsAttr).

constraints_for_vars(Vars, Constraints, InvolvedVarsAttr) :-
    nb_getval(constraints, AllConstraints),
    vars_to_vars_attr(Vars, VarsAttr),
    list_to_set(VarsAttr, VarsAttrSet),
    constraints_for_vars_aux(VarsAttrSet, Constraints, InvolvedVarsAttr, AllConstraints).


% clear_constraints_for_vars(+Vars)
clear_constraints_for_vars_aux(_, [], []).
clear_constraints_for_vars_aux(VarsAttr, [Constraint|Constraints], RemainingConstraints) :-
    Constraint = constraint(_, InvolvedVarsAttr),
    clear_constraints_for_vars_aux(VarsAttr, Constraints, RemainingConstraints1),
    (intersection(VarsAttr, InvolvedVarsAttr, []) ->
        RemainingConstraints = [Constraint|RemainingConstraints1];
        RemainingConstraints = RemainingConstraints1).

clear_constraints_for_vars(Vars) :-
    nb_getval(constraints, AllConstraints),
    vars_to_vars_attr(Vars, VarsAttr),
    clear_constraints_for_vars_aux(VarsAttr, AllConstraints, RemainingConstraints),
    nb_setval(constraints, RemainingConstraints).


% add_constraint(+Constraint, +InvolvedVarsAttr)
add_constraint(Constraint, InvolvedVarsAttr) :-
    % CAUTION: nb does not work with backtracking
    nb_getval(constraints, Constraints),
    nb_setval(constraints, [constraint(Constraint, InvolvedVarsAttr)|Constraints]).


% serialize_expr(+Expr, -ParsedExpr, -InvolvedVarUuids)
serialize_expr(Var, Res, [var(Uuid, Lb, Ub)]) :-
    fd_var(Var),
    get_remote_clp_attr(Var, Uuid, Lb, Ub),
    Res = json([type=variable, value=Uuid]).
serialize_expr(Literal, Res, []) :-
    integer(Literal),
    Res = json([type=literal, value=Literal]).
serialize_expr(Expr, ParsedExpr, VarUuids) :-
    nonvar(Expr),
    Expr =.. [Op, A, B],
    member(Op, ['+', '-', '*', '//', 'mod']),
    serialize_expr(A, ParsedA, VarUuidsA),
    serialize_expr(B, ParsedB, VarUuidsB),
    union(VarUuidsA, VarUuidsB, VarUuids),
    ParsedExpr = json([type=operator, value=Op, children=[ParsedA, ParsedB]]).


% add_arithmetic_constraint(+Op, +A, +B)
add_arithmetic_constraint(Op, A, B) :-
    ((var(A), \+ fd_var(A)) ->
        A in inf..sup ;
        true),
    ((var(B), \+ fd_var(B)) ->
        B in inf..sup ;
        true),

    serialize_expr(A, ParsedA, VarsA),
    serialize_expr(B, ParsedB, VarsB),
    union(VarsA, VarsB, Vars),

    uuid(Uuid, [version(4)]),
    ParsedExpr = json([
        id=Uuid,
        type=arithmetic,
        value=json([
            type=operator,
            value=Op,
            children=[ParsedA, ParsedB]])]),
    add_constraint(ParsedExpr, Vars).


% serialize_options(+Options, -OptionsJson)
serialize_options([], []).
serialize_options([Option|Options], [OptionJson|OptionsJson]) :-
    Option =.. [Op, Var],
    member(Op, [min, max]),
    \+ member(min(_), Options),
    \+ member(max(_), Options),
    get_remote_clp_attr(Var, Uuid, _, _),
    OptionJson = json([value=Uuid, type=Op]),
    serialize_options(Options, OptionsJson).
serialize_options([time_limit(H, M, S, MS)|Options], [json([value=Timeout, type=time_limit])|OptionsJson]) :-
    integer(H), integer(M), integer(S), integer(MS),
    \+ member(time_limit(_, _, _, _), Options),
    Timeout is ((H*60 + M) * 60 + S) * 1000 + MS,
    serialize_options(Options, OptionsJson).
serialize_options([solution_limit(N)|Options], [json([value=N, type=solution_limit])|OptionsJson]) :-
    integer(N),
    \+ member(solution_limit(_), Options),
    serialize_options(Options, OptionsJson).


% check_status(+StatusUrl, -Results)
check_result(StatusUrl, Data, Error) :-
    http_get(StatusUrl, json(Ls), []),
    member(status=Status, Ls),
    write('job status: '), write(Status), nl,
    Status = done,
    % TODO: not the entire response
    member(data=Data, Ls),
    member(error=Error, Ls).


% http_solve(+Constraints)
http_solve(Json, Data) :-
    setting(api_url, Url),
    atom_concat(Url, '/jobs', JobsUrl),

    http_post(
        JobsUrl,
        json(Json),
        json(Ls),
        [headers(Headers)]),
    member(status=Status, Ls),
    member(location(ResultsPath), Headers),
    write('job status: '), write(Status), nl,
    write('job results path: '), write(ResultsPath), nl,

    atom_concat(Url, ResultsPath, ResutlsUrl),
    thread_wait(check_result(ResutlsUrl, Data, Error), [retry_every(1)]),

    writeln('job done'),
    (Error \= '' -> (writeln(Error), fail) ; true).


% unify_solution(+JsonVars, +Vars)
unify_solution(_, []).
unify_solution(JsonVars, [Var|Vars]) :-
    get_remote_clp_attr(Var, Uuid, _, _),
    member('='(Uuid, Val), JsonVars),
    Var = Val,
    unify_solution(JsonVars, Vars).


% fd_var(+Var)
fd_var(Var) :- get_remote_clp_attr(Var, _, _, _).


% labeling(+Options, +Vars)
labeling(Options, Vars) :-
    constraints_for_vars(Vars, Constraints, InvolvedVarsAttr),

    serialize_vars(InvolvedVarsAttr, VarsJson),

    uuid(Uuid, [version(4)]),
    JsonAttrs = [id=Uuid, variables=VarsJson, constraints=Constraints],

    (Options = [] ->
        JsonAttrs1 = JsonAttrs ;
        % TODO: rename to serialize
        (serialize_options(Options, OptionsJson), JsonAttrs1 = [options=OptionsJson|JsonAttrs])),

    Json = json(JsonAttrs1),
    http_solve(Json, Solutions),

    clear_constraints_for_vars(Vars),

    member(json(SolutionVars), Solutions),
    unify_solution(SolutionVars, Vars).

% labeling(+Vars)
label(Vars) :- labeling([], Vars).


% +Var in +Lb .. +Ub
Var in Lb .. Ub :-
    valid_domain(Lb, Ub),
    (fd_var(Var) -> 
        get_remote_clp_attr(Var, Uuid, _, _) ;
        uuid(Uuid, [version(4)])),
    put_remote_clp_attr(Var, Uuid, Lb, Ub).

% +Vars ins +Ub .. +Lb
[] ins _ .. _.
[Var|Vars] ins Lb .. Ub :-
    Var in Lb .. Ub,
    Vars ins Lb .. Ub.


% all_different(+Vars)
all_different(Vars) :-
    vars_to_vars_attr(Vars, VarsAttr),
    vars_to_uuids(Vars, Uuids),
    uuid(Uuid, [version(4)]),
    Json = json([id=Uuid, type=all_different, value=Uuids]),
    add_constraint(Json, VarsAttr).


% +Var #= +Expr
A #= B :- add_arithmetic_constraint(==, A, B).
% +Var #\= +Expr
A #\= B :- add_arithmetic_constraint('!=', A, B).
% +Var #> +Expr
A #> B :- add_arithmetic_constraint(>, A, B).
% +Var #< +Expr
A #< B :- add_arithmetic_constraint(<, A, B).
% +Var #>= +Expr
A #>= B :- add_arithmetic_constraint(>=, A, B).
% +Var #=< +Expr
A #=< B :- add_arithmetic_constraint(<=, A, B).


_ #<==> _ :- fail.
_ #==> _ :- fail.
_ #<== _ :- fail.
_ #\/ _ :- fail.
_ #\ _ :- fail.
_ #/\ _ :- fail.
#\ _ :- fail.
