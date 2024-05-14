:- module(remote-clp).

% hook (callback) that is called when a variable is unified
% these hooks are bound to the module so they don't interfere with other attributes
attr_unify_hook(M, E) :- 
    writeln('Variable was unified with:'),
    write('value: '), writeln(E),
    write('had rclp attribute: '), writeln(M).

define([], _).
define([Var|Ls], N) :-
    % add the rclp attribute to the variable with value N
    put_attr(Var, rclp, N),
    N1 is N + 1,
    define(Ls, N1).

define(Ls) :-
    define(Ls, 1).

solve([]).
solve([Var|Ls]) :-
    % get the rclp attribute from the variable
    get_attr(Var, rclp, N),
    writeln(N),
    solve(Ls).

go :-
    define([X,Y,Z]),
    solve([X,Y]),
    Z = a.
