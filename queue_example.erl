-module(queue_example).
-export([main/0]).

main() ->
    % Create a new empty queue
    Q = queue:new(),

    % Add elements to the queue
    Q1 = queue:in("first", Q),
    Q2 = queue:in("second", Q1),
    Q3 = queue:in_r("zero", Q2),

    % Print the queue as a list
    io:format("Queue: ~p~n", [queue:to_list(Q3)]).
