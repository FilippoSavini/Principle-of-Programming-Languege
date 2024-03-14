% Create a distributed hash table with separate chaining. The hash table will consist of an agent for each
% bucket, and a master agent that stores the buckets’ PIDs and acts as a middleware between them and the
% user. Actual key/value pairs are stored into the bucket agents.
% The middleware agent must be implemented by a function called hashtable_spawn that takes as its
% arguments (1) the hash function and (2) the number of buckets. When executed, hashtable_spawn
% spawns the bucket nodes, and starts listening for queries from the user. Such queries can be of two kinds:
% • Insert: {insert, Key, Value} inserts a new element into the hash table, or updates it if an
% element with the same key exists;
% • Lookup: {lookup, Key, RecipientPid} sends to the agent with PID “RecipientPid” a
% message of the form {found, Value}, where Value is the value associated with the given key, if
% any. If no such value exists, it sends the message not_found.
% The following code:
% main() ->
%  HT = spawn(?MODULE, hashtable_spawn, [fun(Key) -> Key rem 7 end, 7]),
%  HT ! {insert, 15, "Apple"},
%  HT ! {insert, 8, "Orange"},
%  timer:sleep(500),
%  HT ! {lookup, 8, self()},
%  receive
%  {found, A1} -> io:format("~s~n", [A1])
%  end,
%  HT ! {insert, 8, "Pineapple"},
%  timer:sleep(500),
%  HT ! {lookup, 8, self()},
%  receive
%  {found, A2} -> io:format("~s~n", [A2])
%  end.
% should print the following:
% Orange
% Pineapple
-module('20220121').
-export([main/0, hashtable_spawn/2, spawn_bucket/2, listen_user_query/2, bucket/1]).

hashtable_spawn(Hf, NBuck) ->
    Ps = spawn_bucket(NBuck, []),
    listen_user_query(Hf, Ps).


spawn_bucket(0, Pids) -> Pids;
spawn_bucket(NBuck, Pids) ->
    Pid = spawn(?MODULE, bucket, [[]]),
    spawn_bucket(NBuck-1, Pids ++ [Pid]).

listen_user_query(Hf, Pids) ->
    receive
        {insert, Key, Value} ->
            lists:nth(Hf(Key) + 1, Pids) ! {insert, Key, Value},
            listen_user_query(Hf, Pids);
        {lookup, Key, RecipientPid} ->
            lists:nth(Hf(Key) + 1, Pids) ! {lookup, Key, RecipientPid, self()},
            listen_user_query(Hf, Pids);
        {not_found, Key, RecipientPid} ->
            RecipientPid ! {not_found, Key},
            listen_user_query(Hf, Pids);
        {found, Value, RecipientPid} ->
            RecipientPid ! {found, Value},
            listen_user_query(Hf, Pids)
        end.

bucket(Tuples) ->
    receive
        {insert, Key, Value} ->
            case lists:keyfind(Key, 1, Tuples) of
                false -> bucket([{Key, Value} | Tuples]);
                {_, _} -> bucket(lists:keyreplace(Key, 1, Tuples, {Key, Value}))
            end,
            bucket(Tuples);
        {lookup, Key, RecipientPid, From} ->
            case lists:keyfind(Key, 1, Tuples) of
                false -> From ! {not_found, Key, RecipientPid};
                {_, Value} -> From ! {found, Value, RecipientPid}
            end,
            bucket(Tuples)
        end.

main() ->
    HT = spawn(?MODULE, hashtable_spawn, [fun(Key) -> Key rem 7 end, 7]),
    HT ! {insert, 15, "Apple"},
    HT ! {insert, 8, "Orange"},
    timer:sleep(500),
    HT ! {lookup, 8, self()},
    receive
        {found, A1} -> io:format("~s~n", [A1])
    end,
    HT ! {insert, 8, "Pineapple"},
    timer:sleep(500),
    HT ! {lookup, 8, self()},
    receive
        {found, A2} -> io:format("~s~n", [A2])
    end.