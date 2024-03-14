% Consider a main process which takes two lists: one of function names, and one of lists of parameters (the
% first element of with contains the parameters for the first function, and so forth). For each function, the
% main process must spawn a worker process, passing to it the corresponding argument list. If one of the
% workers fails for some reason, the main process must create another worker running the same function.
% The main process ends when all the workers are done.
-module('20210714').
-export([create_worker/3, main/2, main_loop/2, add/2, fact/2]).


create_worker([], [], Pids) -> Pids;
create_worker(Functions, Argument, Pids) ->
    [F | Func] = Functions,
    [P | Param] = Argument,
    Worker = spawn_link(?MODULE, F, P),
    io:format("Worker ~p created~n", [Worker]),
    create_worker(Func, Param, Pids#{Worker => {F, P}}).
    

main(L1, L2) ->
    process_flag(trap_exit, true),
    Workers = create_worker(L1, L2, #{}),
    main_loop(Workers, length(L1)).

main_loop(Workers, Count) ->
    receive
        {'EXIT', Worker, normal} ->
            io:format("Worker ~p has ended ~n", [Worker]),
            if
                Count =:= 1 -> ok;
                true -> main_loop(Workers, Count-1)
            end;
        {'EXIT', Worker, _} ->
            #{Worker := {F, P}} = Workers,
            NewWorker = spawn_link(?MODULE, F, P),
            io:format("Worker ~p has died, new worker ~p~n", [Worker, NewWorker]),
            main_loop(Workers#{NewWorker => {F, P}}, Count)
    end.


add(X, Y) ->
    io:format("The reault of add is ~p~n", [X + Y]).

fact(0, _) -> 1;
fact(1, Acc) -> io:format("The Result of fact is ~p~n", [Acc]);
fact(N, Acc) ->
    fact(N-1, Acc*N).





