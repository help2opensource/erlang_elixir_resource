-module(mnesia_example).
-export([start/0, stop/0, write_person/1, read_person/1, delete_person/1]).

-record(person, {id, name, age}).

%% Start Mnesia and create schema and table
start() ->
    %% Start the Mnesia application
    mnesia:start(),

    %% Create schema on the current node
    mnesia:create_schema([node()]),

    %% Create the 'person' table with attributes: id, name, and age
    mnesia:create_table(person, [{attributes, record_info(fields, person)}]),

    ok.

%% Stop Mnesia
stop() ->
    mnesia:stop().

%% Write a new person record to the 'person' table
write_person(#person{id = Id, name = Name, age = Age}) ->
    Fun = fun() ->
        mnesia:write(#person{id = Id, name = Name, age = Age})
    end,
    mnesia:transaction(Fun).

%% Read a person record by id from the 'person' table
read_person(Id) ->
    Fun = fun() ->
        case mnesia:read({person, Id}) of
            [] -> {error, not_found};
            [Record] -> {ok, Record}
        end
    end,
    mnesia:transaction(Fun).

%% Delete a person record by id from the 'person' table
delete_person(Id) ->
    Fun = fun() ->
        mnesia:delete({person, Id})
    end,
    mnesia:transaction(Fun).
