:- use_module('remote_clp').

% set the URL of the remote CLP server (default is 'http://localhost:3000')
% :- initialization(api_config([url('http://localhost:3000')])).

pyth_triplets(N,Ls1) :- 
    Ls1 = [A1,B1,C],
    Ls = [U,V,K, A,B,C], 
    rclp_fd_domain(Ls,1,N),
	A*A + B*B ~#= C*C,
	UU ~#= U*U, 
	VV ~#= V*V,
	(U-V) rem 2 ~#= 1,
	coprime(U,V, Aux),
	A ~#= K*(UU - VV),
	C ~#= K*(UU + VV),
	B ~#= K*(2*U*V),
	append([UU, VV|Ls], Aux, AllVariables),
	rclp_fd_labeling(AllVariables), 
	unbreak_symmetry(A,B, A1,B1).

coprime(A,B, [X,Y]) :-
	X~#=<B, Y~#=<A, 
	(A*X - B*Y ~#= 1).

unbreak_symmetry(A,B, A,B).
unbreak_symmetry(A,B, B,A).

% ------------------------------

% --- maximize:   (3a + 2b)
%     subject to: a+b<50, 4a-b<88, a,b in [0..1000]
optimizeDemo(A,B) :- 
        rclp_fd_domain([A,B],0, 50),
            Z ~#= 3*A + 2*B,
          A+B ~#< 50,
        4*A-B ~#< 88,
        rclp_fd_maximize(rclp_fd_labeling([A,B]), Z).
