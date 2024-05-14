:- use_module('../remote_clp').

:- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_token>')]).


pyth_triplets(N,Ls1) :-
    Ls1 = [A1,B1,C],
    Ls = [A,B,C], 
    Ls ins 1..N,
    A #=< B, B #=< C,
    A*A + B*B #= C*C,
    label(Ls),
    unbreak_symmetry(A,B,A1,B1).


unbreak_symmetry(A,B, A,B).
unbreak_symmetry(A,B, B,A).

