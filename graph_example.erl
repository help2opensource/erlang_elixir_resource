-module(graph_example).
-export([main/0]).

main() ->
    % Create a new directed graph
    Graph = digraph:new(),

    % Add vertices (nodes)
    V1 = digraph:add_vertex(Graph, v1),
    V2 = digraph:add_vertex(Graph, v2),
    V3 = digraph:add_vertex(Graph, v3),

    % Add edges (directed connections)
    digraph:add_edge(Graph, V1, V2),
    digraph:add_edge(Graph, V2, V3),
    digraph:add_edge(Graph, V1, V3),

    % Display vertices and edges
    io:format("Vertices: ~p~n", [digraph:vertices(Graph)]),
    io:format("Edges: ~p~n", [digraph:edges(Graph)]),

    % Find successors of V1
    SuccessorsV1 = digraph:out_neighbours(Graph, V1),
    io:format("Successors of V1: ~p~n", [SuccessorsV1]),

    % Find the shortest path from V1 to V3
    Path = digraph:get_short_path(Graph, V1, V3),
    io:format("Shortest path from V1 to V3: ~p~n", [Path]),

    % Clean up: delete the graph
    digraph:delete(Graph).
