open Graph

(**)
val arc_cost: int graph -> int arc -> int

val bellman_ford: int graph -> int graph -> id -> id -> (id list * int array)