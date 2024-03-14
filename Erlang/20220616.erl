% Define a program tripart which takes a list, two values x and y, with x < y, and three functions, taking
% one argument which must be a list.
% • tripart first partitions the list in three sublists, one containing values that are less than both x and
% y, one containing values v such that x ≤ v ≤ y, and one containing values that are greater than both
% x and y.
% • Three processes are then spawned in parallel, running the three given functions and passing the
% three sublists in order (i.e. the first function must work on the first sublist and so on).
% • Lastly, the program must wait the termination of the three processes in the spawning order,
% assuming that each one will return the pair {P, V}, where P is its PID and V the resulting value.
% • tripart must return the three resulting values in a list, with the resulting values in the same order
% as the corresponding sublists.

-module('20220616').
-export([helper/6, part/3, tripart/6]).


helper([Elem | L], X, Y, L1, L2, L3) when (Elem < X) and (Elem < Y) -> helper([L], X, Y, [Elem | L1], L2, L3);
helper([Elem | L], X, Y, L1, L2, L3) when (Elem >= X) and (Elem =< Y) -> helper([L], X, Y, L1, [Elem | L2], L3);
helper([Elem | L], X, Y, L1, L2, L3) when (Elem > X) and (Elem > Y) -> helper([L], X, Y, L1, L2, [Elem | L3]);
helper([], _, _, L1, L2, L3) -> [L1, L2, L3].

part(L, X, Y) ->
    helper (L, X, Y, [], [], []).

tripart(L, X, Y, F, G, H) ->
    [D1, D2, D3] = part(L, X, Y),
    P1 = spawn(?MODULE, F, [D1]),
    P2 = spawn(?MODULE, G, [D2]),
    P3 = spawn(?MODULE, H, [D3]),
    receive
        {P1, V1} ->
            receive
                {P2, V2} ->
                    receive
                        {P3, V3} ->
                            [V1, V2, V3]
                    end
            end
    end.






