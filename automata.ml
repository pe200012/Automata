open Core_kernel

type ('arg,'result) t = (int, ('arg,'result) node) Hashtbl.t
and ('arg,'result) node =
  {
    nodetype : ('arg,'result) nodetype;
    comment   : string
  }
and ('arg,'result) nodetype =
  | Inital of ('arg -> int * 'arg)
  | Node   of ('arg -> int * 'arg)
  | Final  of ('arg -> 'result)

exception Unknown_node
exception Illegal_action of string
exception Exn_pair of exn * exn

let create (ns:(int*(_,_)node)list) : (_,_) t =
  let ta = Int.Table.create () in
  List.iter ns (fun (i,n) -> Hashtbl.add_exn ~key:i ~data:n ta);
  ta

let run (init_f:unit->int*_) ?(debug=false) (t:(_,_) t) =
  let rec loop n a =
    match n.nodetype with
    | Final f -> f a
    | Inital f | Node f ->
      trans (try f a with e -> raise (Exn_pair (Illegal_action "Terminated on non-final node",e))) 
  and trans (i,a) =
    match Hashtbl.find t i with
    | None -> raise Unknown_node
    | Some n -> loop n a
  in
  trans (init_f ())

