open Graph

(* Renvoie graphe d'écart associé à un graphe contenant flots et capacités *)
val gap_graph: (int * int) graph -> int graph

(* Fonction créant un graphe avec flot = capacité/2 pour tests *)
val test_graph: int graph -> (int * int) graph

(* Renvoie le graphe de flot associé à un graphe d'écart (flots positifs et négatifs) sous la forme "(flot/capa)" *)
val flow_graph: id graph -> id graph -> string graph

(* Renvoie le graphe de flot associé à un graphe d'écart (flots positifs et négatifs) sous la forme "flot" *)
val flow_graph_no_parentheses: id graph -> id graph -> string graph

(* Fonction renvoyant le flot a ajouter sur le graphe a la prochaine iteration *)
val get_min_capa : int graph -> id list -> int

(* Fonction mettant a jour le graphe d'écart en fonction du chemin trouvé *)
val update_graph: int graph -> id list -> id -> int graph