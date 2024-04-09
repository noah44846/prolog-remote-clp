:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

check :-
    http_open('http://localhost:3000/status', In, []),
    json_read(In, Json, [true(t), false(f)]),
    close(In),
    write('status: '), write(Json), write(' '), get_time(T), write(T), nl,
    Json = t.

polling :-
    write('Starting'), nl,
    http_open('http://localhost:3000/solve', In, []),
    close(In),
    thread_wait(check, [retry_every(1)]),
    write('Fetching result'), nl,
    http_open('http://localhost:3000/value', In2, []),
    json_read(In2, Json),
    close(In2),
    write(Json), nl,
    write('Done'), nl.
