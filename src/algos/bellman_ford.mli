open Graph

(* Fonction renvoyant le coût d'un arc *)
val arc_cost: int graph -> int arc -> int

(* Fonction renvoyant le meilleur chemin de s à p trouvé par bellan Ford *)
val bellman_ford: int graph -> int graph -> id -> id -> (id list * int array)