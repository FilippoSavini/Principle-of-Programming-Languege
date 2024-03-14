%Write a concurrent Erlang program that simulates the previous FSA, where each state is implemented as a process.
%S -> aS | bbS | bbc

-module('20230125').
-export([q0/0, q1/0, q2/0, q3/0, q4/0, start/0, read_string/1]).

q0() ->
    receive
        {[a | Xs]} -> q0 ! {Xs};
        {[b | Xs]} -> 
            q1 ! {Xs},
            q2 ! {Xs}
    end,
    q0().

q1() ->
    receive
        {[b | Xs]} -> q0 ! {Xs}
    end,
    q1().

q2() ->
    receive
        {[b | Xs]} -> q3 ! {Xs}
    end,
    q2().

q3() ->
    receive
        {[c | Xs]} -> q4 ! {Xs}
    end,
    q3().

q4() ->
    receive
        {[]} -> io:format("String accepted")
    end,
    q4().

start() ->
    register(q0, spawn(fun() -> q0() end)), % to avoid exporting qs
    register(q1, spawn(fun() -> q1() end)),
    register(q2, spawn(fun() -> q2() end)),
    register(q3, spawn(fun() -> q3() end)),
    register(q4, spawn(fun() -> q4() end)).

read_string(S) ->
    q0 ! {S}.