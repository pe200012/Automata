open Core_kernel

type ('stat,'arg,'result) t = (int, ('stat,'arg,'result) node) Hashtbl.t
and ('stat,'arg,'result) node =
  {
    nodetype : ('stat,'arg,'result) nodetype;
    comment   : string
  }
and ('stat,'arg,'result) nodetype =
  | Inital of ('arg -> 'stat) * ('stat -> int * 'arg)
  | Node   of ('arg -> 'stat) * ('stat -> int * 'arg)
  | Final  of ('arg -> 'stat) * ('stat -> 'result)

exception Unknown_node
exception Illegal_action of string
exception Exn_pair of exn * exn

let create (ns:(int*(_,_,_)node)list) =
  let ta = Int.Table.create () in
  List.iter ns (fun (i,n) -> Hashtbl.add_exn ~key:i ~data:n ta);
  ta

let run init_f ?(debug=false) t =
  let rec loop n arg =
    match n.nodetype with
    | Final (action, result) ->
      (if debug = true then Printf.printf "[DEBUG]Final Node %s\n" n.comment);
      result (action arg)
    | Inital (action, trans) | Node (action, trans) ->
      let i, a = try trans (action arg) with e -> raise (Exn_pair (Illegal_action "Terminated on non-final node",e)) in
      match Hashtbl.find t i with
      | None -> raise Unknown_node
      | Some n ->
        (if debug = true then Printf.printf "[DEBUG]Node %d:%s\n" i n.comment);
        loop n a
  in
  let i, a = init_f () in
  match Hashtbl.find t i with
  | None -> raise Unknown_node
  | Some n ->
    match n.nodetype with
    | Inital (_, _) -> 
      (if debug = true then Printf.printf "[DEBUG]Start from inital node %d:%s\n" i n.comment);
      loop n a
    | _ -> raise (Illegal_action "Start from a non-inital node")

