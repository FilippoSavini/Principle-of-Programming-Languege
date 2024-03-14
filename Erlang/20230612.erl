% Consider the following implementation of mergesort and write a parallel version of it.
% mergesort([L]) -> [L];
% mergesort(L) ->
%  {L1, L2} = lists:split(length(L) div 2, L),
%  merge(mergesort(L1), mergesort(L2)).
% merge(L1, L2) -> merge(L1, L2, []).
% merge([], L2, A) -> A ++ L2;
% merge(L1, [], A) -> A ++ L1;
% merge([H1|T1], [H2|T2], A) when H2 >= H1 -> merge(T1, [H2|T2], A ++ [H1]);
% merge([H1|T1], [H2|T2], A) when H1 > H2 -> merge([H1|T1], T2, A ++ [H2]).

-module('20230612').
-export([start/1, mergesort/2, merge/2, merge/3]).

start(L) ->
    Me = self(),
    mergesort(L, self()),
    receive
        {Me, R} -> R
    end.

mergesort([L], Pid) ->
    Pid ! {self(), [L]};
mergesort(L, Pid) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    P1 = spawn(?MODULE, mergesort, [L1, self()]),
    P2 = spawn(?MODULE, mergesort, [L2, self()]),
    receive
        {P1, Res1} ->
            receive
                {P2, Res2} ->
                    Pid ! {self(), merge(Res1, Res2)}
            end 
    end.

merge(L1, L2) -> merge(L1, L2, []).
merge([], L2, A) -> A ++ L2;
merge(L1, [], A) -> A ++ L1;
merge([H1|T1], [H2|T2], A) when H2 >= H1 -> merge(T1, [H2|T2], A ++ [H1]);
merge([H1|T1], [H2|T2], A) when H1 > H2 -> merge([H1|T1], T2, A ++ [H2]).
