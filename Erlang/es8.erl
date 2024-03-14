-module(es8).
-export([pmap/2, execute/3, test/0,
        check/3, pfilter/2, test1/0,
        dofold/3, partition/2, parthelp/6, parfold/3,
        listlink/2, master/1, master_loop/2]).

% parallel map take a function and a list spawn a process execute for each elem of L with as arg the 2 input and wait for the resul from all
pmap(F, L) ->
    Ps = [spawn(?MODULE, execute, [F, X, self()])|| X <- L],
    [receive
        {Pid, X} -> X
    end || Pid <- Ps].

% execute spawned from pmap execute the f and send back the result
execute(F, X, Pid) ->
    Pid ! {self(), F(X)}.


test() ->
    pmap(fun (X) -> X*X end, [1,2,3,4]).


% send to pid the result of a func and its argument
check(P, X, Pid) ->
    Pid ! {self(), P(X), X}.
    

% pfilter take a func and a list, spawn a process check for every elem in L with as argument the func the list its pid then filter true responce
pfilter(P, L) ->
    Ps = [spawn(?MODULE, check, [P, X, self()])|| X <- L],
    lists: foldl(fun (Pid, Vo) ->
                        receive
                            {Pid, true, X} -> Vo ++ [X];
                            {Pid, false, _} -> Vo
                        end
                end, [], Ps).


test1() ->
    pfilter(fun (X) -> X > 2 end, [1,2,3,4]).


% partition take a list and the size of each partition and prepare the call of parthelp, Chunk number of partition
partition(L, N) -> 
   M = length(L),
   Chunk = M div N,
   End = M - Chunk*(N-1),
   parthelp(L, N, 1, Chunk, End, []).

% parthelp case no partition  put in res the list
parthelp(L, 1, P, _, E, Res) ->
    Res ++ lists: sublist(L, P, E);
% case partition concat in Res the sublist
parthelp(L, N, P, C, E, Res) ->
    R = lists: sublist(L, P, C),
    parthelp(L, N-1, P+C, C, E, Res ++ [R]).
    
% parfold call partition then spawn process dofold for each elem of the partition with as arg pid f and the elem then wait for the respnce and prepare the arg of a fold
parfold(F, L, N) ->
    Ls = partition(L, N),
    Ps = [spawn (?MODULE, defold, [self(), F, X])|| X <- Ls],
    [R|Rs] = [receive
                  {P, V} -> V 
              end || P <- Ps],
    lists:foldl(F, R, Rs).

% dofold recive a pid a func and a list and send back Pid and the foldl list
dofold(Pid, F, [X|Xs]) ->
    Pid ! {self(), list:foldl(F, X, Xs)}.


listlink([], Pids) -> Pids;
listlink([F|Fs], Pids) ->
    Pid = spawn_link(F),
    listlink(Fs, Pids#{Pid => F}).


master(Functions) ->
    process_flag(trap_exit, true),
    Workers = listlink(Functions, #{}),
    master_loop(Workers, length(Functions)).


master_loop(_, 0) ->
    ok; 
master_loop(Workers, Count) ->
    receive
        {'EXIT', _, normal} ->
            master_loop(Workers, Count-1);
        {'EXIT', Pid, _} ->
            #{Pid := Fun} = Workers,
            Child = spawn_link(Fun),
            master_loop(Workers#{Child => Fun}, Count)
    end.
