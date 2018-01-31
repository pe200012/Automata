module Automata : sig

  exception Unknown_node
  exception Illegal_action of string
  exception Exn_pair of exn * exn

  module type S = sig
    (** The interface of automachine *)

    (** type of automachine of node *)
    type ('stat,'arg,'result) t
    type ('stat,'arg,'result) node

    (** [[create ns]] will create a automachine with type of ns's nodes' type *)
    val create  : ('a * ('a,'b,'c) node) list -> ('a,'b,'c) t

    val add     : ('a,'b,'c) t -> ('a * ('a,'b,'c) node) -> unit

    val replace : ('a,'b,'c) t -> ('a * ('a,'b,'c) node) -> unit

    val remove  : ('a,'b,'c) t -> 'a -> unit

    val clear   : ('a,'b,'c) t -> unit

    (** [[run t]] will eventually return a result with type of t's result type. 
        Will raise exception if the node except {b Final} raise a exception
        @param debug default = false. Will print some infos if set.
    *)
    val run     : (unit -> 'a * 'b) -> ('a,'b,'c) t -> 'c
  end

  module A1 : sig
    type ('stat,'arg,'result) node =
      {
        f : [ `Node of 'arg -> 'stat * 'arg | `Final of 'arg -> 'result];
        comment : string (** For debug purpose *)
      }
    include S with type ('stat,'arg,'result) node := ('stat,'arg,'result) node
    val run  : (unit -> 'a * 'b) -> ?debug:bool -> ('a,'b,'c) t -> 'c
  end

end
