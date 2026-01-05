open Graph

(**)
val arc_cost: int graph -> int arc -> int -> int

(**)
val dijkstra_path: int graph -> int graph -> id -> id -> id list

(**)
val successive_shortest_path: (int * int) graph -> id -> id -> string graph
