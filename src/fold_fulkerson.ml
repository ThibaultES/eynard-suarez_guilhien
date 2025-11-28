open Graph
open Tools

let gap_graph graph = (* gr : graphe dont les labels sont des tuples (flux, capacite) *)
  e_fold graph  ( 
    fun g2 arc ->  (match arc.lbl with
      | p, c when p = 0 && c > 0 -> Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = c}
      | p, c when p = c && p > 0 -> Graph.new_arc g2 {src = arc.tgt; tgt = arc.src; lbl = p}
      | p, c when p > 0 -> let gnewarc = Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = (p - c)} in
        Graph.new_arc gnewarc {src = arc.tgt; tgt = arc.src; lbl = p}
      | _,_ -> g2
      )
  )  (clone_nodes graph)


let test_graph graph =
  gmap graph (fun id -> (id / 2, id))