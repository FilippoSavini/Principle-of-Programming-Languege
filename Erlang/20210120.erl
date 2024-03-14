% Define a function for a proxy used to avoid to send PIDs; the proxy must react to the following messages:

% - {remember, PID, Name}: associate the value Name with PID.

% - {question, Name, Data}: send a question message containing Data to the PID corresponding
%   to the value Name (e.g. an atom), like in PID ! {question, Data}

% - {answer, Name, Data}: send an answer message containing Data to the PID corresponding
%   to the value Name (e.g. an atom), like in PID ! {answer, Data}

-module('20210120').
-export([proxy/1]).

proxy(Table) ->
    receive
        {remember, PID, Name} ->
            proxy(Table#{Name => PID});
        {question, Name, Data} ->
            #{Name := Id} = Table,
            Id ! {question, Data},
            proxy(Table);
        {answe, Name, Data} ->
            #{Name := Id} = Table,
            Id ! {answer, Data},
            proxy(Table)
    end.




