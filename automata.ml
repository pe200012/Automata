open Core_kernel

module Automata = 
struct

  exception Unknown_node
  exception Illegal_action of string
  exception Exn_pair of exn * exn

  module type S = sig
    type ('stat,'arg,'result) t
    type ('stat,'arg,'result) node
    val create  : ('a * ('a,'b,'c) node) list -> ('a,'b,'c) t
    val add     : ('a,'b,'c) t -> ('a * ('a,'b,'c) node) -> unit
    val replace : ('a,'b,'c) t -> ('a * ('a,'b,'c) node) -> unit
    val remove  : ('a,'b,'c) t -> 'a -> unit
    val clear   : ('a,'b,'c) t -> unit
    val run     : (unit -> 'a * 'b) -> ('a,'b,'c) t -> 'c
  end

  module A1 : sig
    type ('stat,'arg,'result) node =
      {
        f : [ `Node of 'arg -> 'stat * 'arg | `Final of 'arg -> 'result];
        comment : string
      }
    include S with type ('stat,'arg,'result) node := ('stat,'arg,'result) node
    val run  : (unit -> 'a * 'b) -> ?debug:bool -> ('a,'b,'c) t -> 'c
  end = struct

    (** 自动机和节点类型 *)
    type ('stat,'arg,'result) t = ('stat, ('stat,'arg,'result) node) Hashtbl.Poly.t
    (** 使用哈希表来组织自动机，相对列表提高查询效率 *)
    and ('stat,'arg,'result) node =
      {
        f : [ `Node of 'arg -> 'stat * 'arg | `Final of 'arg -> 'result];
        comment   : string
      }

    let create (ns:('a*(_,_,_)node)list) =
      let t = Hashtbl.Poly.create () in
      List.iter ~f:(fun (key,data) ->
          match Hashtbl.add t ~key ~data with
          | `Duplicate -> raise (Illegal_action "Duplicated node")
          | `Ok -> ()) ns;
      t

    let add t n =
      let s, n = n in
      match Hashtbl.add t ~key:s ~data:n with
      | `Duplicate -> raise (Illegal_action "Duplicated node")
      | `Ok -> ()

    let replace t n = let key, data = n in Hashtbl.set t ~key ~data

    let remove = Hashtbl.Poly.remove

    let clear = Hashtbl.Poly.clear

    let run (init_f:unit -> 'a * 'b) ?(debug=false) (t:('a,'b,'c)t) : 'c =
      let rec loop n a =
        match n.f with
        | `Final f -> f a
        | `Node f ->
          trans (try f a with e -> raise (Exn_pair (Illegal_action "Terminated on non-final node",e)))
      and trans (s,a) =
        match Hashtbl.Poly.find t s with
        | None -> raise Unknown_node
        | Some n -> loop n a
      in
      trans (init_f ())
  end

end
