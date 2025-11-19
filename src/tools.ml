open Graph

let clone_nodes gr = assert false
;;

let gmap gr f = 
  e_fold gr  (fun g2 arc ->  (Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = (f arc.lbl)}))  (clone_nodes gr)
;;

