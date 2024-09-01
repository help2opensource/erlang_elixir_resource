-module(array_example).
-export([main/0]).

main() ->
    % Create an array with size 10, initialized with default value 'undefined'
    Array = array:new(10),

    % Set value at index 2
    Array1 = array:set(2, 42, Array),

    % Get value at index 2
    Value = array:get(2, Array1),

    % Print the value
    io:format("Value at index 2: ~p~n", [Value]).
