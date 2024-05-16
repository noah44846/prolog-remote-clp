:- use_module('./remote_clp').
:- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_key>')]).

:- begin_tests(remote_clp_simple).

test(simple) :-
    Ls = [A, B],
    Ls ins 0..1,
    A #< B,
    label(Ls),
    assertion(Ls == [0, 1]).

:- end_tests(remote_clp_simple).


:- begin_tests(remote_clp_n_queens).
:- ['./examples/n_queens'].

test(n_queens_correct_solutions) :-
    n_queens(4, Queens4),
    Solutions4 = [[2, 4, 1, 3], [3, 1, 4, 2]],
    assertion(member(Queens4, Solutions4)),

    n_queens(5, Queens5),
    Solutions5 = [[2, 4, 1, 3, 5], [2, 5, 3, 1, 4], [1, 3, 5, 2, 4], [3, 1, 4, 2, 5], [5, 2, 4, 1, 3],
                 [1, 4, 2, 5, 3], [5, 3, 1, 4, 2], [4, 1, 3, 5, 2], [4, 2, 5, 3, 1], [3, 5, 2, 4, 1]],
    assertion(member(Queens5, Solutions)).
   
test(n_queens_correct_number_of_solutions) :-
    findall(Queens, n_queens(4, Queens), S4),
    assertion(length(S4, 2)),
    findall(Queens, n_queens(5, Queens), S5),
    assertion(length(S5, 10)),
    findall(Queens, n_queens(6, Queens), S6),
    assertion(length(S6, 4)),
    findall(Queens, n_queens(7, Queens), S7),
    assertion(length(S7, 40)),
    findall(Queens, n_queens(8, Queens), S8),
    assertion(length(S8, 92)),
    findall(Queens, n_queens(9, Queens), S9),
    assertion(length(S9, 352)).

:- end_tests(remote_clp_n_queens).

:- begin_tests(remote_clp_pythagorean_triple).
:- ['./examples/pythagorean_triples'].

test(pythagorean_triple_solutions) :-
    pyth_triplets(20, Triplets),
    Solutions = [[3, 4, 5], [4, 3, 5], [5, 12, 13], [12, 5, 13], [6, 8, 10], [8, 6, 10],
                 [8, 15, 17], [15, 8, 17], [9, 12, 15], [12, 9, 15], [12, 16, 20], [16, 12, 20]],
    assertion(member(Triplets, Solutions)).

test(pythagorean_triple_number_of_solutions) :-
    findall(Triplets, pyth_triplets(20, Triplets), S20),
    assertion(length(S20, 12)),
    findall(Triplets, pyth_triplets(50, Triplets), S50),
    assertion(length(S50, 40)),
    findall(Triplets, pyth_triplets(500, Triplets), S500),
    assertion(length(S500, 772)).

:- end_tests(remote_clp_pythagorean_triple).

:- begin_tests(remote_clp_optimization).
:- ['./examples/simple_optimization'].

test(simple_optimization) :-
    optimizeDemo(Ls),
    assertion(Ls == [27,22]).

:- end_tests(remote_clp_optimization).

:- begin_tests(remote_clp_word_puzzle).
:- ['./examples/word_puzzle'].

test(word_puzzle) :-
    solve_puzzle(Ls),
    assertion(Ls == [5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18]).

:- end_tests(remote_clp_word_puzzle).
