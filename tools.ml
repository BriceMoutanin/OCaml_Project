
let clone_nodes g = n_fold g new_node empty_graph

let gmap gr f =
  let gr2 = clone_nodes gr in
  (*ligne pour convertir les arcs*)
  e_fold gr new_arc gr2
