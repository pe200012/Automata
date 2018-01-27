open Core

type ('stat,'arg) t =
  {
    inital_func : unit -> int * 'arg;
    nodes : ('stat,'arg) node list
  }
and ('stat,'arg) node =
  {
    index : int;
    nodetype : nodetype;
    action : ('arg -> 'stat);
    trans_fun : ('stat -> int * 'arg);
    comment : string
  }
and nodetype =
  [
    | `Inital
    | `Node
    | `Final
  ]
and ('stat,'arg,'result) anode =
  {
    anodetype : ('stat,'arg,'result) anodetype
  }
and ('stat,'arg,'result) anodetype =
  | Inital of ('arg -> 'stat) * ('stat -> int * 'arg)
  | Node   of ('arg -> 'stat) * ('stat -> int * 'arg)
  | Final  of ('arg -> 'stat) * ('stat -> 'result)

exception Unknown_node
exception Illegal_action of string
exception Exn_pair of exn * exn

let create init_f ns =
  {
    inital_func = init_f;
    nodes = ns
  }

let run t =
  let i, a = t.inital_func () in
  match List.find t.nodes (fun x -> x.index = i) with
  | None -> raise Unknown_node
  | Some n ->
    match n.nodetype with 
    | `Inital ->
      let current = ref n in
      let carg = ref a in
      while true do
        match (!current).nodetype with
        | `Final -> ignore ((!current).action !carg); ()
        | `Inital | `Node ->
          try
            let index, arg' = (!current).trans_fun ((!current).action !carg) in
            match List.find t.nodes (fun x -> x.index = index) with
            | None -> raise Unknown_node
            | Some n ->
              current := n;
              carg := arg'
          with 
          | e -> raise (Exn_pair (Illegal_action "Terminated on non-final node", e))
      done
    | _ -> raise (Illegal_action "Start from a non-inital node")

