(** 自动机和节点类型 *)
type ('arg,'result) t
type ('arg,'result) node =
  {
    nodetype : ('arg,'result) nodetype;
    comment   : string (** 调试时信息 *)
  }
and ('arg,'result) nodetype =
  | Inital of ('arg -> int * 'arg)
  | Node   of ('arg -> int * 'arg)
  | Final  of ('arg -> 'result)

exception Unknown_node
exception Illegal_action of string
exception Exn_pair of exn * exn

(** [create ns] will create a automachine with type of ns's nodes' type *)
val create : (int * ('a, 'b) node) list -> ('a, 'b) t

(** [[run t]] will eventually return a result with type of t's result type. 

@ param debug default = false. Will print some infos if set.

Will raise exception if the node except {b Final} raise a exception *)
val run : (unit -> int * 'a) -> ?debug:bool -> ('a, 'b) t -> 'b

