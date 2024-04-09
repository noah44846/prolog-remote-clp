module(rclp).

:- op(760, yfx, ~#<==>).
:- op(750, xfy, ~#==>).
:- op(750, yfx, ~#<==).
:- op(740, yfx, ~#\/).
:- op(730, yfx, ~#\).
:- op(720, yfx, ~#/\).
:- op(710,  fy, ~#\).
:- op(700, xfx, ~#>).
:- op(700, xfx, ~#<).
:- op(700, xfx, ~#>=).
:- op(700, xfx, ~#=<).
:- op(700, xfx, ~#=).
:- op(700, xfx, ~#\=).
% :- op(700, xfx, in).
% :- op(700, xfx, ins).
% :- op(450, xfx, ..). % should bind more tightly than \/
% :- op(150, fx, #).

attr_unify_hook(M, E) :- 
    writeln('Variable was unified with:'),
    write('value: '), writeln(E),
    write('had rclp attribute: '), writeln(M).

define([], _).
define([Var|Ls], N) :-
    put_attr(Var, rclp, N),
    N1 is N + 1,
    define(Ls, N1).

define(Ls) :-
    define(Ls, 1).

solve([]).
solve([Var|Ls]) :-
    get_attr(Var, rclp, N),
    writeln(N),
    solve(Ls).

go :-
    define([X,Y,Z]),
    solve([X,Y]),
    Z = a.
