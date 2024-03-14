% Define a function which takes two list of PIDs [x1, x2, ...], [y1, y2, ...], having the same length, and a
% function f, and creates a different "broker" process for managing the interaction between each pair of
% processes xi and yi.
% At start, the broker process i must send its PID to xi and yi with a message {broker, PID}. Then, the
% broker i will receive messages {from, PID, data, D} from xi or yi, and it must send to the other one an
% analogous message, but with the broker PID and data D modified by applying f to it.
% A special stop message can be sent to a broker i, that will end its activity sending the same message to xi
% and yi. 

-module('20210831').
-export([main/3, broker/3]).

main(Func, Lx, Ly) ->
    process_flag(trap_exit, true),
    [spawn_link(?MODULE, broker, [Func, X, Y]) || X <- Lx, Y <- Ly].
    

broker(F, X, Y) ->
    register(broker, self()),
    X ! {broker, self()},
    Y ! {broker, self()},
    receive
        {from, PID, data, D} ->
            if 
                PID == X ->
                    Y ! {from, self(), data, F(D)};
                true ->
                    X ! {from, self(), data, F(D)}
            end,
            broker(F, X, Y);
        stop ->
            X ! stop,
            Y ! stop
    end.




