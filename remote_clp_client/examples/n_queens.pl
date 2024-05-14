%:- use_module(library(clpfd)).
:- use_module('../remote_clp').

:- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_token>')]).


go(Ls) :-
    call_time(findall(Qs, n_queens(12, Qs), Ls), Time),
    write('Time: '), writeln(Time.wall).


n_queens(N, Qs) :-
    length(Qs, N),
    Qs ins 1..N,
    all_different(Qs),
    diag(Qs, Ds1, Ds2),
    all_different(Ds1),
    all_different(Ds2),
    append(Qs, Ds1, Vs1),
    append(Vs1, Ds2, Vs),
    label(Vs).

diag([], [], []). 
diag(Qs, Ds1, Ds2) :-
    diag(Qs, Ds1, Ds2, 0).

diag([], [], [], _).
diag([Q|Qs], [D1|Ds1], [D2|Ds2], N) :-
    D1 #= Q + N,
    D2 #= Q - N,
    N1 is N + 1,
    diag(Qs, Ds1, Ds2, N1).

