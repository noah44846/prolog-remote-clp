:- use_module('../remote_clp').

pyth_triplets(N,Ls1) :- 
    Ls1 = [A1,B1,C],
    Ls = [U,V,K, A,B,C], 
    Ls ins 1..N,
	A*A + B*B #= C*C,
	UU #= U*U, 
	VV #= V*V,
	(U-V) mod 2 #= 1,
	coprime(U,V, Aux),
	A #= K*(UU - VV),
	C #= K*(UU + VV),
	B #= K*(2*U*V),
	append([UU, VV|Ls], Aux, AllVariables),
	label(AllVariables), 
	unbreak_symmetry(A,B, A1,B1).

coprime(A,B, [X,Y]) :-
	X#=<B, Y#=<A, 
	(A*X - B*Y #= 1).

unbreak_symmetry(A,B, A,B).
unbreak_symmetry(A,B, B,A).
