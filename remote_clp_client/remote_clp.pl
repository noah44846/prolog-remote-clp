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
    label/1,
    labeling/2
]).

:- use_module(library(uuid)).
:- use_module(library(settings)).
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
    integer(Lb) ; Lb = inf,
    integer(Ub) ; Ub = sup.


% get_remote_clp_attr(+Var, -Uuid, -Ub, -Lb)
get_remote_clp_attr(Var, Uuid, Lb, Ub) :-
    get_attr(Var, remote_clp, var(Uuid, Lb, Ub)),
    uuid_property(Uuid, version(4)).


% put_remote_clp_attr(+Var, +Uuid, +Ub, +Lb)
put_remote_clp_attr(Var, Uuid, Lb, Ub) :-
    put_attr(Var, remote_clp, var(Uuid, Lb, Ub)).


% parse_vars(+Vars, -VarsDict)
parse_vars([], []).
parse_vars([Var|Ls1], [Json|Ls2]) :-
    get_remote_clp_attr(Var, Uuid, Lb, Ub),
    uuid_property(Uuid, version(4)),
    JsonAttrs = [id=Uuid],
    (Lb = inf -> JsonAttrs1 = JsonAttrs ; JsonAttrs1 = [lb=Lb|JsonAttrs]),
    (Ub = sup -> JsonAttrs2 = JsonAttrs1 ; JsonAttrs2 = [ub=Ub|JsonAttrs1]),
    Json = json(JsonAttrs2),
    parse_vars(Ls1, Ls2).


% get_constraints(-Constraints)
get_constraints(Constraints) :-
    nb_getval(constraints, Constraints).


% add_constraint(+Constraint)
add_constraint(Constraint) :-
    % CAUTION: nb does not work with backtracking
    nb_getval(constraints, Constraints),
    nb_setval(constraints, [Constraint|Constraints]).


% parse_expr(+Expr, -ParsedExpr)
parse_expr(Var, Res) :-
    fd_var(Var),
    get_remote_clp_attr(Var, Uuid, _, _),
    Res = json([type=variable, value=Uuid]).
parse_expr(Literal, Res) :-
    integer(Literal),
    Res = json([type=literal, value=Literal]).
parse_expr(Expr, ParsedExpr) :-
    nonvar(Expr),
    Expr =.. [Op, A, B],
    member(Op, ['+', '-', '*']),
    parse_expr(A, ParsedA),
    parse_expr(B, ParsedB),
    ParsedExpr = json([type=operator, value=Op, children=[ParsedA, ParsedB]]).


% parse_objectives(+Options, -Objectives)
parse_objectives([], []).
parse_objectives([Option|Options], [Objective|Objectives]) :-
    Option =.. [Op, Var],
    member(Op, ['min', 'max']),
    get_remote_clp_attr(Var, Uuid, _, _),
    Objective = json([value=Uuid, type=Op]),
    parse_objectives(Options, Objectives).


% add_arithmetic_constraint(+Op, +A, +B)
add_arithmetic_constraint(Op, A, B) :-
    parse_expr(A, ParsedA),
    parse_expr(B, ParsedB),
    uuid(Uuid, [version(4)]),
    ParsedExpr = json([
        id=Uuid,
        type=linear,
        value=json([
            type=operator,
            value=Op,
            children=[ParsedA, ParsedB]])]),
    add_constraint(ParsedExpr).


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
unify_solution1(_, []).
unify_solution1(JsonVars, [Var|Vars]) :-
    get_remote_clp_attr(Var, Uuid, _, _),
    member('='(Uuid, Val), JsonVars),
    Var = Val,
    unify_solution1(JsonVars, Vars).


% fd_var(+Var)
fd_var(Var) :- get_remote_clp_attr(Var, _, _, _).


% labeling(+Options, +Vars)
labeling(Options, Vars) :-
    parse_vars(Vars, VarsJson),
    
    get_constraints(Constraints),
    % TODO: clear constraints ?
    nb_setval(constraints, []),

    uuid(Uuid, [version(4)]),
    JsonAttrs = [id=Uuid, variables=VarsJson, constraints=Constraints],

    (Options = [] ->
        JsonAttrs1 = JsonAttrs ;
        (parse_objectives(Options, Obj), JsonAttrs1 = [objectives=Obj|JsonAttrs])),

    Json = json(JsonAttrs1),
    http_solve(Json, Solutions),

    member(json(SolutionVars), Solutions),
    unify_solution1(SolutionVars, Vars).

% label(+Vars)
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


% +Var #= +Expr
A #= B :-
    ((var(A), \+ (var(B), fd_var(B))) ->
        A in inf..sup ;
        true),
    ((var(B), \+ (var(A), fd_var(A))) ->
        B in inf..sup ;
        true),
    
    add_arithmetic_constraint(==, A, B).
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
