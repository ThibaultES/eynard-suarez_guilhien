open Graph

(* Renvoie graphe d'écart associé à un graphe contenant flots et capacités : largement inutilisée *)
val gap_graph: (int * int) graph -> int graph

(* Fonction créant un graphe avec flot = capacité/2 pour teterfonction au dessus : largement inutilisée *)
val test_graph: int graph -> (int * int) graph

val flow_graph: id graph -> id graph -> string graph

val flow_graph_no_parentheses: id graph -> id graph -> string graph

(* Fonction renvoyant le flot a ajouter sur le graphe a la prochaine iteration *)
val get_min_capa : int graph -> id list -> int

(* Fonction mettant a jour le graphe d'écart en fonction du chemin trouvé *)
val update_graph: int graph -> id list -> id -> int graph