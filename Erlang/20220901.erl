% We want to implement a parallel foldl, parfold(F, L, N), where the binary operator F is associative, and N
% is the number of parallel processes in which to split the evaluation of the fold. Being F associative,
% parfold can evaluate foldl on the N partitions of L in parallel. Notice that there is no starting (or
% accumulating) value, differently from the standard foldl.
% You may use the following libray functions:
% lists:foldl(<function>, <starting value>, <list>)
% lists:sublist(<list>, <init>, <length>), which returns the sublist of <list> starting at position
% <init> and of length <length>, where the first position of a list is 1.
-module('20220901').
-export([helper/6, parfold/3, exec/2]).

helper(L, 1, Init, _, End, Out) -> 
    Out ++ lists:sublist(L, Init, End);

helper(L, N, Init, Chunck, End, Out) ->
    R = lists:sublist(L, Init, Chunck),
    helper(L, N-1, Init + Chunck, Chunck, End, Out ++ [R]).


parfold(F, L, N) ->
    register(master, self()),
    M = length(L),
    Chunck = M div N,
    End = M - Chunck*(N-1),
    Part = helper(L, N, 1, Chunck, End, []),
    Pids = [spawn(?MODULE, exec, [F, D]) || D <- Part],
    [R | Rs] = [receive {From, V} -> V end || From <- Pids],
    lists:foldl(F, R, Rs).


exec(F, D) ->
    master ! {self(), lists:foldl(F, 0, D)}.
