% Define a parallel lexer, which takes as input a string x and a chunk size n, and translates all the words in
% the strings to atoms, sending to each worker a chunk of x of size n (the last chunk could be shorter than
% n). You can assume that the words in the string are separated only by space characters (they can be more
% than one - the ASCII code for ' ' is 32); it is ok also to split words, if they overlap on different chunks.
% E.g.
%  plexer("this is a nice test", 6) returns [[this,i],[s,a,ni],[ce,te],[st]]
% For you convenience, you can use the library functions:
% • lists:sublist(List, Position, Size) which returns the sublist of List of size Size from
% position Position (starting at 1);
% • list_to_atom(Word) which translates the string Word into an atom. 

-module('20220210').
%-export([plexer/2, listen_responce/3, plex_helper/6, atom/1]).

% plexer(X, Size) ->
%     M = length(X),
%     Chunk = M div Size,
%     End = M - Size*(Chunk-1),
%     Res = plex_helper(X, Size, 1, Chunk, End, []),
%     Proc = [spawn(?MODULE, atom, [R]) || R <- Res],
%     N = length(Proc),
%     listen_responce(N, [], self()),
%     receive
%         {final, L} ->
%             io:format("~p", L)
%     end.

% listen_responce(N, L, Pid) when N > 1  ->
%     register(listener, self()),
%     receive
%         {responce, L1} ->
%             L ++ L1,
%             listen_responce(N-1, L, Pid)
%         end;

% listen_responce(_, L, Pid) -> 
%     Pid ! {final, L}.



% plex_helper(L, _, P, 1, End, Res) ->
%     Res ++ lists: sublist(L, P, End);
% plex_helper(L, Size, P, Chunk, End, Res) ->
%     R = lists: sublist(L, P, Size),
%     plex_helper(L, Size, P+Size, Chunk-1, End, Res ++ [R]).

% atom(L) ->
%     SubString = string:split(L, " ", all),
%     Tmp = [list_to_atom(Word) || Word <- SubString],
%     listener ! {responce, Tmp}.
-export([split/4, lex/2, run/2, plex/2]).

split(List, Size, Pos, End) when Pos < End ->
    [lists:sublist(List, Pos, Size)] ++ split(List, Size, Pos+Size, End);
split(_, _, _, _) -> [].

lex([X|Xs], []) when X =:= 32 ->
    lex(Xs, []);
lex([X|Xs], Word) when X =:= 32 ->
     [list_to_atom(Word)] ++ lex(Xs, []);
lex([X|Xs], Word) ->
     lex(Xs, Word++[X]);
lex([], []) ->
     [];
lex([], Word) ->
     [list_to_atom(Word)].

run(Pid, Data) ->
     Pid!{self(),
    lex(Data, [])}.

plex(List, Size) ->
     Part = split(List, Size, 1, length(List)),
     W = lists:map(fun(X) ->
                    spawn(?MODULE, run, [self(), X])
                end, Part),
         lists:map(fun (P) ->
            receive
                {P, V} -> V
            end
        end, W).

