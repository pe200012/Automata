type ('stat,'arg) t

type ('stat,'arg) node =
  {
    index : int;
    nodetype : nodetype;
    action : ('arg -> 'stat);
    trans_fun : ('stat -> int * 'arg);
    comment : string
  }
and nodetype =
  | Inital
  | Node
  | Final

exception Unknown_node
exception Illegal_action of string
exception Exn_pair of exn * exn

val create : (unit -> int * 'arg) -> ('stat,'arg) node list -> ('stat,'arg) t
val run : (_,_) t -> unit
