% Write a concurrent Erlang program that simulates only the given PDA, and each state of the PDA is implemented
% as an independent parallel process.

% S -> aA
% A -> aA | bC | bA 
-module('20230215').
-export([q0/0, q1/0, q2/0, q3/0, q4/0, q5/0, start/0, stop/0, read_string/1]).

q0() ->
    receive
        {[a | Xs], ["Z"]} -> 
            q1 ! {Xs, ["A" | "Z"]}
    end,
    q0().

q1() ->
    receive
        {[a | Xs], ["A" | S]} ->
            New = ["A", "A"],
            q1 ! {Xs, New ++ S};
        {[b | Xs], ["A" | S]} ->
            q2 ! {Xs, S},
            q3 ! {Xs, ["A" | S]}
    end,
    q1().

q2() ->
    receive
        {[b | Xs], ["A" | S]} ->
            q2 ! {Xs, S};
        {[], ["Z"]} ->
            q5 ! {[], []}
    end,
    q2().

q3() ->
    receive
        {[b | Xs], ["A" | S]} ->
            q4 ! {Xs, S}
    end,
    q3().

q4() ->
    receive
        {[b | Xs], ["A"| S]} ->
            q3 ! {Xs, ["A" | S]};
        {[], ["Z"]} ->
            q5 ! {[], []}
    end,
    q4().

q5() ->
    receive
        {[], []} ->
            io:format("String accepted")
    end.


start() ->
    register(q0, spawn(fun() -> q0() end)),
    register(q1, spawn(fun() -> q1() end)),
    register(q2, spawn(fun() -> q2() end)),
    register(q3, spawn(fun() -> q3() end)),
    register(q4, spawn(fun() -> q4() end)),
    register(q5, spawn(fun() -> q5() end)).

read_string(S) ->
    q0 ! {S, ["Z"]}.

stop() ->
    unregister(q0),
    unregister(q1),
    unregister(q2),
    unregister(q3),
    unregister(q4),
    unregister(q5).