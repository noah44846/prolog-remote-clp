:- use_module('../remote_clp').

% :- api_config([
%     url('https://remote-clp.kube.isc.heia-fr.ch/api'),
%     key('<token>')]).

% --- maximize:   (3a + 2b)
%     subject to: a+b<50, 4a-b<88, a,b in [0..1000]
optimizeDemo(A,B) :- 
    [A,B] ins 0..50,
    Z #= 3*A+2*B,
    A+B #< 50,
    4*A-B #< 88,
    labeling([max(Z)], [A,B]).
