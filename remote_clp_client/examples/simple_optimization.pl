:- use_module('../remote_clp').

:- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_token>')]).

go(Ls) :- 
    Ls = [A,B],
    Ls ins 0..50,
    Z #= 3*A+2*B,
    A+B #< 50,
    4*A-B #< 88,
    labeling([max(Z)], [A,B]).

