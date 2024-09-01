-module(set_example).
-export([main/0]).

main() ->
    % Create individual sets
    Set1 = sets:from_list([1, 2, 3]),
    Set2 = sets:from_list([4, 5, 6]),
    % This set is identical to Set1
    Set3 = sets:from_list([1, 2, 3]),

    % Create a new set that will hold sets (convert sets to lists first)
    SetOfSets = sets:new(),
    SetOfSets1 = sets:add_element(Set1, SetOfSets),
    SetOfSets2 = sets:add_element(Set2, SetOfSets1),
    % This will not add as Set1 and Set3 are identical
    SetOfSets3 = sets:add_element(Set3, SetOfSets2),

    % Print the resulting set of sets
    io:format("Set of Sets: ~p~n", [SetOfSets3]),

    Set1A = ordsets:from_list([1, 2, 3]),
    Set2A = ordsets:from_list([4, 5, 6]),
    % This set is identical to Set1
    Set3A = ordsets:from_list([1, 2, 3]),

    % Create a set of sets (list of ordsets)
    SetOfSetsA = ordsets:from_list([Set1A, Set2A, Set3A]),

    % Print the resulting set of sets (duplicates removed)
    io:format("Set of Sets (Ordsets): ~p~n", [SetOfSetsA]),

    SetB = gb_sets:new(),

    % Insert elements into the set
    Set1B = gb_sets:insert(1, SetB),
    Set2B = gb_sets:insert(2, Set1B),
    Set3B = gb_sets:insert(3, Set2B),

    % Print the current set
    io:format("Set after inserting 1, 2, 3: ~p~n", [gb_sets:to_list(Set3B)]),

    % Create a family (set of sets)
    S1 = sofs:relation([{a, 1}, {b, 2}]),
    S2 = sofs:relation([{x, 3}, {y, 4}]),
    S = sofs:from_sets([S1, S2]),
    sofs:to_external(S).
