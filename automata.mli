type ('arg,'result) t
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

val create : (int * ('a, 'b) node) list -> ('a, 'b) t
val run : (unit -> int * 'a) -> ?debug:bool -> ('a, 'b) t -> 'b

