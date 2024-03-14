-module('20230703').
-export([start/1, deeprev/2]).

start(L) ->
    deeprev(self(), L),
    receive
        {P, R} -> R 
    end.

deeprev(P, []) ->
    P ! {self(), []};
deeprev(P, [X | Xs]) ->
    Pid1 = spawn(?MODULE, deeprev, [self(), X]),
    Pid2 = spawn(?MODULE, deeprev, [self(), Xs]),
    receive
        {Pid1, V1} ->
            receive
                {Pid2, V2} ->
                    P ! {self(), V2 ++ [V1]}
            end
    end;
deeprev(P, X) -> P ! {self(), X}.


