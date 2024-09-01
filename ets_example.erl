-module(ets_example).
-export([start/0]).

start() ->
    %% Step 1: Create an ETS table
    Table = ets:new(my_table, [set, named_table, public]),

    %% Step 2: Insert key-value pairs into the table
    ets:insert(Table, {name, "Alice"}),
    ets:insert(Table, {age, 30}),
    ets:insert(Table, {city, "New York"}),

    %% Step 3: Lookup data by key
    Name = ets:lookup(Table, name),
    io:format("Name: ~p~n", [Name]),

    %% Step 4: Delete a specific key
    ets:delete(Table, age),

    %% Step 5: Try to lookup deleted key
    Age = ets:lookup(Table, age),
    io:format("Age: ~p~n", [Age]),

    %% Step 6: Delete the entire table
    ets:delete(Table).

    
