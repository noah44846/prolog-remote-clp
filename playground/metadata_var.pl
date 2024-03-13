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
    write(M), writeln(E).

define([]).
define([Var|Ls]) :-
    put_attr(Var, rclp, id),
    define(Ls).

solve([]).
solve([Var|Ls]) :-
    get_attr(Var, rclp, id),
    solve(Ls).


go :-
    define([X,Y,Z]),
    % do some stuff
    solve([X,Y,Z]).
