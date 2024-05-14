%:- use_module(library(clpfd)).
:- use_module('../remote_clp').

:- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_token>')]).


go(Ls) :-
    call_time(findall(L, optimizeDemo(L), Ls), Time),
    write('Time: '), writeln(Time.wall).


optimizeDemo(Ls) :- 
    Ls = [A,B],
    Ls ins 0..50,
    Z #= 3*A+2*B,
    A+B #< 50,
    4*A-B #< 88,
    labeling([max(Z)], [A,B]).
