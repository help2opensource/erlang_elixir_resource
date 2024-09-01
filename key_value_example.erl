-module(key_value_example).
-export([main/0]).

main() ->
    % Example with proplist
    Proplist = [{name, "Alice"}, {age, 30}],
    Value = proplists:get_value(name, Proplist),
    io:format("Value: ~p~n", [Value]),

    % Example with orddict
    Orddict = orddict:store(name, "Alice", orddict:from_list([])),
    Value1 = orddict:fetch(name, Orddict),
    io:format("Value1: ~p~n", [Value1]),

    % Example with dict
    Dict = dict:store(name, "Alice", dict:from_list([])),
    Value2 = dict:find(name, Dict),
    io:format("Value2: ~p~n", [Value2]),

    % Example with gb_trees
    Tree = gb_trees:empty(),
    Tree1 = gb_trees:insert(name, "Alice", Tree),
    Tree2 = gb_trees:insert(age, 30, Tree1),
    Value3 = gb_trees:lookup(name, Tree2),
    io:format("Value3: ~p~n", [Value3]).
