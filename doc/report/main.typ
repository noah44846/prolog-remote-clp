// ---------------------------------------------------------------------------
// Copyright © 2024 Haute école d'ingénierie et d'architecture de Fribourg
// SPDX-License-Identifier: Apache-2.0
// ---------------------------------------------------------------------------
// Author : Jacques Supcik <jacques.supcik@hefr.ch>
// Date   : 23 February 2024
// ---------------------------------------------------------------------------
// Example of a student reports at the Haute école d'ingénierie et
// d'architecture de Fribourg
// ---------------------------------------------------------------------------

#import "lib/heiafr.typ": report, code_block
#import "@preview/big-todo:0.2.0": *
#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex

#set text(region: "ch", lang: "en")

// N.B. : The versions array needs to be ordered from the oldest to the newest
#let versions = (
  (
    version: "0.1",
    date: datetime(year: 2024, month: 03, day: 11),
    changes: [First version],
  ),
  (
    version: "0.2",
    date: datetime(year: 2024, month: 03, day: 11),
    changes: [Corrected some typos, added references and add design diagrams],
  ),
)

#show: doc => report(
  lang: "en",
  type: [Semester 6 project],
  year: [2024],
  departement: [Department of Computer Science],
  profile: [Software Engineering Orientation],
  title: [
    Prolog remote constraint logic programming
  ],
  subtitle: [
    Technical documentation
  ],
  authors: (
    (firstname: "Noah", lastname: "Godel", gender: "h"),
  ),
  supervisors: ("Frédéric Bapst",),
  versions: versions,
  theme_color: rgb(0,124,183,255),
  doc,
)

= Introduction

This report presents the activities carried out during the semester 6 project of the Software Engineering Bachelor program at the School of Engineering and Architecture of Fribourg. The next section provides an overview of the project. For further details, please refer to the specification @spec_doc. The project repository is located at the following URL.

#align(center, [
    https://gitlab.forge.hefr.ch/frederic.bapst/24-ps6-remote-clp
])

== Context

The purpose of this project is to develop a solution that bridges the gap between Prolog and Operations Research (OR) libraries, such as Google OR Tools and Gecode. The goal is to create a web-based system that allows multiple users to access and utilize these OR libraries for Constraint Logic Programming (CLP) in Prolog.  

Previous attempts @prev_proj to integrate Prolog with OR libraries, such as Google OR Tools, required local installation of the libraries, limiting accessibility. This project aims to overcome these limitations by developing a web-based API that eliminates the need for local installations. The proposed architecture leverages the scalability and resilience of a Kubernetes cluster to host the web API, making it well-suited for deployment in various operational scenarios.

== Goal

The primary goals of this project are:

- Implement a web API that provides access to the CP-SAT solver of the Google OR Tools library for CLP in Prolog. The API should be designed to handle multiple concurrent requests and provide scalability for increased usage in the future. A token-based authentication mechanism will be implemented to ensure secure access to the API.
- Deploy the web API on a Kubernetes cluster to ensure scalability and resilience, with automated deployment using Gitlab CI/CD pipelines.
- Develop an OS-independent client library for SWI-Prolog that allows easy access to the web API, in a way similar to other CLP libraries in Prolog.
- Perform a series of tests, including unit tests and performance tests, to ensure the reliability and performance of the web API.
- Implement a series of demonstration programs to showcase the capabilities of the web API and the client library.

== Document structure

This document is structured as follows.

- _Introduction_: this section presents the context, the goal and the structure of the document.
- _Analysis_: this section presents the constraints, exploration of different technologies and features and the technological choices.
- _Design_: this section presents the design of the Prolog client library and the remote CLP service.
- _Implementation_: this section presents the implementation of the Prolog client library and the remote CLP service.
- _Results_: this section presents the challenges and the future work.
- _Conclusion_: this section concludes the document.

= Analysis

This section presents the constraints, the exploration of different technologies and features and the technological choices.

== Constraints

This project has several constraints that need to be taken into account during the development process. One of them is that the client library must work cross-platform. SWI-Prolog can be installed on all the most popular operating systems (Windows, macOS, Linux) and the client library should work in the same way on all of them. This means that no platform specific code should be used in the client library.

This project additionally has the following constraints.

=== Authentication

To insure the security of the service, the client must authenticate itself. Since a constraint logic programming service can be quite compute intensive, the service must be able to identify the client and limit the number of requests.

To implement this, we will use a token based authentication system. The user (the administrator to be specific) will request a token from the service and use it in all subsequent requests. The rate of requests will be limited by the token.

To manage the tokens an administration interface need to be implemented. This interface will allow the administrator to create, delete and list the tokens.

=== Concurrency

As already mentioned, the service can be quite compute intensive. One of the constraints of this project is to be able to handle multiple requests concurrently.

It is also specified that the service will be deployed on the institution's kubernetes cluster. This means that the service must be able to scale horizontally. A simple way to achieve this is to use a message queue to distribute the requests to multiple workers.

To ensure that the message queue doesn't overfill, the rate limiting system will be implemented at the API Gateway level.

=== Prolog library usage

The client library must feel similar to use as other CLP libraries that are available in Prolog. The structure of the library is inspired by the clpfd library @swi_clpfd_web. The usage of the API must be hidden from the user as much as possible, they only need to specify the token and the host.

Listing @lst_clp_program (inspired by @word_puzzle_clp_code) is an example of how a CLP programs should look like.

#figure(
    code_block[```swi-prolog
    :- use_module(library(clpfd)).

    alpha(Ls) :-
        Ls = [A, B, C, _D, E, F, G, H, I, J, K, L, M,
              N, O, P, Q, R, S, T, U, V, W, X, Y, Z],
        Ls ins 1..26,
        all_distinct(Ls),
        B + A + L + L + E + T #= 45,
        C + E + L + L + O #= 43,
        C + O + N + C + E + R + T #= 74,
        F + L + U + T + E #= 30,
        F + U + G + U + E #= 50,
        G + L + E + E #= 66,
        J + A + Z + Z #= 58,
        L + Y + R + E #= 47,
        O + B + O + E #= 53,
        O + P + E + R + A #= 65,
        P + O + L + K + A #= 59,
        Q + U + A + R + T + E + T #= 50,
        S + A + X + O + P + H + O + N + E #= 134,
        S + C + A + L + E #= 51,
        S + O + L + O #= 37,
        S + O + N + G #= 61,
        S + O + P + R + A + N + O #= 82,
        T + H + E + M + E #= 72,
        V + I + O + L + I + N #= 100,
        W + A + L + T + Z #= 34,
        label(Ls).
    ```],
    caption: [Example of a CLP program],
) <lst_clp_program>

== How Google OR-Tools MathOpt API works

MathOpt @mathopt_web is a library within Google OR-Tools that provides a set of tools to solve mathematical optimization problems. Like linear programming and mixed-integer programming. It makes abstraction of the solver used and provides a common interface to solve the problems.

MathOpt provides a Web API that allows to solve optimization problems remotely on Google's servers. The API is a REST API that uses JSON to encode the optimization problem and the solution.

The usage of the remote API is quite simple. The user creates a problem, sets the variables and constraints as usual. To send it to the remote service, the user must call a remote `solve()` function that takes an API key and the model object. The function returns the solution object.

The model object is formatted in JSON so it can be sent to the API. @lst_mathopt_python_model Python code shows how this works.

#figure(
    code_block[```python
    model = mathopt.Model(name="my_model")
    x = model.add_binary_variable(name="x")
    y = model.add_variable(lb=0.0, ub=2.5, name="y")
    model.add_linear_constraint(x + y <= 1.5, name="c")
    model.add_linear_constraint(2*x + y >= -13, name="d")
    model.maximize(2 * x + y)
    result, logs = remote_http_solve.remote_http_solve(
        model, mathopt.SolverType.GSCIP, api_key=api_key
    )
    ```],
    caption: [Python code to create a MathOpt model],
) <lst_mathopt_python_model>

The code above creates a model with two variables, x and y, and two constraints, c and d. The model is then sent to the remote service and the solution is returned. @lst_mathopt_json_model shows the JSON representation of the model.

#figure(
    code_block[```json
    {
        "solverType": "SOLVER_TYPE_GSCIP",
        "model": {
            "name": "my_model",
            "variables": {
                "ids": [ "0", "1" ],
                "lowerBounds": [ 0.0, 0.0 ],
                "upperBounds": [ 1.0, 2.5 ],
                "integers": [ true, false ],
                "names": [ "x", "y" ]
            },
            "objective": {
                "maximize": true,
                "linearCoefficients": {
                    "ids": [ "0", "1" ],
                    "values": [ 2.0, 1.0 ]
                }
            },
            "linearConstraints": {
                "ids": [ "0", "1" ],
                "lowerBounds": [ "-Infinity", -13.0 ],
                "upperBounds": [ 1.5, "Infinity" ],
                "names": [ "c", "d" ]
            },
            "linearConstraintMatrix": {
                "rowIds": [ "0", "0", "1", "1" ],
                "columnIds": [ "0", "1", "0", "1" ],
                "coefficients": [ 1.0, 1.0, 2.0, 1.0 ]
            }
        }
    }
    ```],
    caption: [JSON representation of a MathOpt model],
) <lst_mathopt_json_model>

== Long running requests in Prolog

Since the solving of constraint logic programming problems can be quite compute intensive, the service must be able to handle long running requests from the client. The service must be able to handle requests that take several minutes to solve.

Handling this with a simple blocking request would not be a good idea. The connection could be lost or the client could timeout. To handle this, the service must be able to handle long running requests in a non-blocking way.

The RESTful way to handle this is to return a 202 Accepted status code with a location header that points to a status endpoint. The client can then poll this endpoint to get the status of the request. When the request is finished, the status endpoint will return a 200 OK status code with the location of the result. @img_long_running_api below shows how this works.

#figure(
  image("img/Long-Running-API.png", width: 70%),
  caption: [Long running requests in a RESTful API],
) <img_long_running_api>

On the Prolog side, the client library must be able to handle this. The client must be able to send a request and poll the status endpoint until the request is finished. When the request is finished, the client must be able to get the result.

SWI-Prolog has built-in libraries to handle HTTP requests, JSON parsing and serialization and threading. This makes it quite easy to implement this in Prolog. @lst_long_running_server shows a Node.js server that simulates a long running request.

#figure(
    code_block[```js
    var http = require('http');

    var value = 0;

    http.createServer((req, res) => {
        if (req.url === '/solve') {
            setTimeout(() => { value = 42; }, 5000);
            res.writeHead(202);
            res.end();
            return;
        } else if (req.url === '/status') {
            res.writeHead(200, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify(value > 0));
            return;
        } else if (req.url === '/value') {
            res.writeHead(200, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify(value));
            return;
        }
    }).listen(3000);
    ```],
    caption: [Node.js server that simulates a long running request],
) <lst_long_running_server>

A Prolog client that polls the status endpoint until the request is finished.
#figure(
    code_block[```swi-prolog
    :- use_module(library(http/http_open)).
    :- use_module(library(http/json)).

    check :-
        http_open('http://localhost:3000/status', In, []),
        json_read(In, Json, [true(t), false(f)]),
        close(In),
        get_time(T),
        write('status: '), write(Json), write(' '), writeln(T),
        Json = t.

    polling :-
        writeln('Starting'),
        http_open('http://localhost:3000/solve', In, []),
        close(In),
        % poll the status endpoint until the request is finished
        thread_wait(check, [retry_every(1)]),
        writeln('Fetching result'),
        http_open('http://localhost:3000/value', In2, []),
        json_read(In2, Json),
        close(In2),
        writeln(Json).
    ```],
    caption: [Prolog client that polls the status endpoint],
) <lst_long_running_prolog>

The output of the Prolog client is shown in @lst_long_running_prolog_output.

#figure(
    code_block[```
    ?- polling.
    Starting
    status: f 1712614154.6943989
    status: f 1712614155.695983
    status: f 1712614156.6980333
    status: f 1712614157.700015
    status: f 1712614158.7022102
    status: t 1712614159.7041478
    Fetching result
    42
    true.
    ```],
    caption: [Output of the Prolog client],
) <lst_long_running_prolog_output>

== Metadata in Prolog variables

To be able to send the variables and constraints to the service, the client library must be able to keep track of metadata in the variables. This metadata is used to identify the variables and the constraints in the service.

@lst_var_metadata_prolog is an example of how this could be implemented in SWI-Prolog.

#figure(
    code_block[```swi-prolog
    % hook (callback) that is called when a variable is unified
    % these hooks are bound to the module so they don't interfere with other attributes
    attr_unify_hook(M, E) :- 
        writeln('Variable was unified with:'),
        write('value: '), writeln(E),
        write('had rclp attribute: '), writeln(M).

    define([], _).
    define([Var|Ls], N) :-
        % add the rclp attribute to the variable with value N
        put_attr(Var, rclp, N),
        N1 is N + 1,
        define(Ls, N1).

    define(Ls) :-
        define(Ls, 1).

    solve([]).
    solve([Var|Ls]) :-
        % get the rclp attribute from the variable
        get_attr(Var, rclp, N),
        writeln(N),
        solve(Ls).

    go :-
        define([X,Y,Z]),
        solve([X,Y]),
        Z = a.
    ```],
    caption: [Prolog variables with metadata],
) <lst_var_metadata_prolog>

The output of the Prolog program is shown in @lst_var_metadata_prolog_output.

#figure(
    code_block[```
    ?- go.
    1
    2
    Variable was unified with:
    value: a
    had rclp attribute: 3
    true.
    ```],
    caption: [Output of the Prolog program],
) <lst_var_metadata_prolog_output>

== Technological choices

In this section, we will present the technological choices that have been made for this project.

=== Prolog engine

The Prolog engine that will be used for this project is SWI-Prolog. SWI-Prolog is a free implementation of Prolog that is widely used in the industry. It has a large set of libraries that make it easy to implement the client library.

In the case of the client for the remote CLP service, SWI-Prolog is also a good choice: It has built-in libraries to handle HTTP requests, JSON parsing and serialization and threading. This makes it easy to implement the client.

However, in the future there is interest in using GNU Prolog since it is used in the institution. This would require a complete rewrite of the client library. From an initial analysis, it seems that GNU Prolog doesn't have built-in libraries to handle HTTP requests directly. This would require to use a C library to handle the requests or to directly use the TCP sockets.

=== Web Service

The API Gateway is the entry point of the service. It is responsible for authenticating the client and rate limiting the requests. It finally forwards the requests to the message queue.

For the API Gateway, a simple but fast language is needed. *Go* @golang with *Fiber* @go_fiber is a good choice for this. It is fast, has a good standard library and is easy to deploy. RESTful APIs are well supported in Go.

The message queue is responsible for distributing the requests to the workers. *Rabbit MQ* @rabbit_mq is a good choice for this. It is a robust message queue that is easy to deploy and has good support for multiple languages.

The workers are responsible for solving the requests. In our case we need to choose a language that is supported by Google OR-Tools. *Python* @python is a good choice for this. It is well supported by Google OR-Tools and is easy to use and deploy.

= Design

This section presents the design of the Prolog client library and the remote CLP service.

== Prolog client library

The Prolog client library is a library that allows the user to access the remote CLP service from Prolog. The library should be easy to use and feel similar to other CLP libraries that are available in Prolog.

=== Structure

=== Usage example

Listings @lst_prolog_example_queens[], @lst_prolog_example_pyth[], @lst_prolog_example_opti[] and @lst_prolog_example_words[] are example programs that show how the client library should be used. These examples will also be used to test the client library.

@lst_prolog_example_queens shows the N-queens problem in Prolog. The program finds a solution to the N-queens problem for a given N. The program uses the `n_queens/2` predicate to find the solution. This program illustrates how to work with lists of variables through the usage of the `all_different/1` and `ins/2` constraints.

#figure(
    code_block[```swi-prolog
    :- use_module('../remote_clp').
    :- api_config([url('https://remote-clp.kube.isc.heia-fr.ch/api'), key('<jwt_token>')]).

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
    ```],
    caption: [N-queens problem in Prolog],
) <lst_prolog_example_queens>

@lst_prolog_example_pyth shows the Pythagorean triplets problem in Prolog. The program finds all Pythagorean triplets with integers below N. The program uses the `pyth_triplets/2` predicate to find the solution. This program illustrates the usage of non-linear constraints (in this case $a^2 + b^2 = c^2$) and the idea of symmetry breaking.

#figure(
    code_block[```swi-prolog
    :- use_module('../remote_clp').
    :- api_config(...).

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
    ```],
    caption: [Pythagorean triplets with integers below N in Prolog],
) <lst_prolog_example_pyth>

@lst_prolog_example_opti shows a simple optimization problem in Prolog. The program finds the values of A and B that maximize the expression $z = 3a + 2b$, subject to the constraints $a + b < 50$ and $4a - b < 88$. The program uses the `optimizeDemo/1` predicate to find the solution. This program illustrates how to work with optimization problems.

#figure(
    code_block[```swi-prolog
    :- use_module('../remote_clp').
    :- api_config(...).

    optimizeDemo(Ls) :- 
        Ls = [A,B],
        Ls ins 0..50,
        Z #= 3*A+2*B,
        A+B #< 50,
        4*A-B #< 88,
        labeling([max(Z)], [A,B]).
    ```],
    caption: [Simple optimization problem in Prolog],
) <lst_prolog_example_opti>

@lst_prolog_example_words shows a word puzzle in Prolog. The numbers 1 - 26 have been randomly assigned to the letters of the alphabet. The numbers beside each word are the total of the values assigned to the letters in the word. e.g for LYRE L,Y,R,E might equal 5,9,20 and 13 respectively or any other combination that add up to 47. The program uses the `solve_puzzle/1` predicate to find the solution. This program illustrates how to work with linear constraints.

#figure(
    code_block[```swi-prolog
    :- use_module('../remote_clp').
    :- api_config(...).

    solve_puzzle(Ls) :-
        Ls = [A, B, C, _D, E, F, G, H, I, J, K, L, M,
              N, O, P, Q, R, S, T, U, V, W, X, Y, Z],
        Ls ins 1..26,
        all_different(Ls),
        B + A + L + L + E + T #= 45,
        C + E + L + L + O #= 43,
        C + O + N + C + E + R + T #= 74,
        F + L + U + T + E #= 30,
        F + U + G + U + E #= 50,
        G + L + E + E #= 66,
        J + A + Z + Z #= 58,
        L + Y + R + E #= 47,
        O + B + O + E #= 53,
        O + P + E + R + A #= 65,
        P + O + L + K + A #= 59,
        Q + U + A + R + T + E + T #= 50,
        S + A + X + O + P + H + O + N + E #= 134,
        S + C + A + L + E #= 51,
        S + O + L + O #= 37,
        S + O + N + G #= 61,
        S + O + P + R + A + N + O #= 82,
        T + H + E + M + E #= 72,
        V + I + O + L + I + N #= 100,
        W + A + L + T + Z #= 34,
        label(Ls).
    ```],
    caption: [Word puzzle in Prolog],
) <lst_prolog_example_words>

== Remote CLP Service

The remote CLP service is a web service that provides access to the CP-SAT solver of the Google OR Tools library for CLP in Prolog. The service should be able to handle multiple concurrent requests and provide scalability for increased usage in the future.
 
=== Kubernetes deployment architecture

@img_deployment_diag shows the deployment architecture of the remote CLP service. The service is deployed on a Kubernetes cluster.

The API Gateway is exposed to the internet through the ingress and is responsible for authenticating the client and sending the requests to the message queue. The message queue distributes the requests to the workers. The workers solve the requests and send the results back to the API Gateway.

#figure(
  image("img/deployment.png", width: 90%),
  caption: [Kubernetes deployment architecture],
) <img_deployment_diag>

=== Client-server communication

@img_protocol_diag shows the communication protocol between the client and the server.

The client sends a request to the API Gateway. The API Gateway authenticates the client and sends the request to the message queue. The message queue distributes the request to the workers. The workers solve the request and send the result back to the API Gateway. The API Gateway sends the result back to the client.

During the process, the client can poll the results endpoint to get the status of the request. When the request is finished, the status endpoint will return the data of the result or an error message.

#figure(
  image("img/protocol_diagram.png", width: 100%),
  caption: [Client-server communication protocol],
) <img_protocol_diag>

=== Request data structure

//#figure(
    //code_block[```json

    //```],
    //caption: [
//) <lst_prolog_example_opti>

=== Result data structure

== Testing strategy

= Implementation

TODO

TODO add details about the solver and its settings (maybe in conception ??):    
- solver limit on model with objective
- only one objective
- ressource leak of Constraints
- automatic cleaning and correct selecting of constraints and variables
- details of how the multiplication and other non-linear constraints work

= Results

= Conclusion

== Challenges

// Channel close for long running requests

// Global constraint list for the client was filling up

// Auth routes where there should not be any

// Duplicate solutions in the solver

== Future work

== Personal opinion

TODO
