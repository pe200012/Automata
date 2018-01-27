type ('stat,'arg,'result) t
type ('stat,'arg,'result) node =
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

val create : (int * ('stat,'arg,'result) node) list -> ('stat,'arg,'result) t
val run : (unit -> int * 'arg) -> ?debug:bool -> (_,'arg,_) t -> unit
