:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

increment :-
    http_open('http://localhost:3000/asfdsd', In, []),
    close(In),
    check.

check :-
    http_open('http://localhost:3000', In, []),
    json_read(In, Json, [true(t), false(f)]),
    close(In),
    write(Json), nl,
    Json = t.

polling :-
    write('Starting'), nl,
    thread_wait(increment, [retry_every(1)]),
    write('Done'), nl.
