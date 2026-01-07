open Graph

(*
PROBLÈME I

Problème : Maximiser le flux de marchandises dans un système de transport modélisé par un graphe.

Entrées : Un graphe G = (S,A) représenté par une liste S de sommets et A d'arcs.
          Un arc de i à j est représenté par un int arc : {src: id ; tgt: id ; lbl: int } où lbl est la capacité maximale de l'arc.

Sortie : Un graphe où les valeurs des arcs correspondent au flux passant dessus pour un flux maximal dans le graphe.

===============
PROBLÈME II

Problème : Maximiser le flux de marchandises dans un système de transport tout en minimisant le coût.

Entrées : Un graphe G = (S,A) représenté par une liste S de sommets et A d'arcs.
          Un arc de i à j est représenté par un arc {src: id ; tgt: id ; lbl: (int, int) }) où lbl = (c, w) avec c la capacité maximale de l'arc
          et w le coût par unité de flux de l'arc.
          Le coût d'un arc sur lequel passe un flux f est donc w*f.

Sortie : Parmis tous les graphes de flux maximal pour cette entrée, celui de coût minimal.
*)

(*  *)
val adapt_input_without_cost : int graph -> id list -> id list -> int graph 

val adapt_input_with_cost: (int * int) graph -> id list -> id list -> (int * int) graph

val remove_additional_nodes: 'a graph -> 'a graph
