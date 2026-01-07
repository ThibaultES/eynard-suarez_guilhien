open Bfs
open Algo_tools

let ford_fulkerson graph s p =
  let rec aux_fulkerson gap_graph = 
  let path = bfs gap_graph s p in match path with
    | [] -> gap_graph
    | _ -> aux_fulkerson (update_graph gap_graph path (get_min_capa gap_graph path))
in flow_graph_no_parentheses graph (aux_fulkerson graph) (* !!! flow_graph_no_parent !!!*)

