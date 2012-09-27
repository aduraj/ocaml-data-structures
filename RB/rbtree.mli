(* Red-black tree *)

type order

module type Compare =
sig
  type t

  val string : t -> string
  val comp : t * t -> order
end

module type RBTree =
sig
  type elem
  type tree

  val empty : unit -> tree
  val isEmpty : tree -> bool
  val insert : elem * tree -> tree
  val delete : elem * tree -> tree
  val member : elem * tree -> bool
    
  val draw : tree -> unit
end

module Int : Compare with type t = int

module Make (El : Compare) : RBTree with type elem = El.t
