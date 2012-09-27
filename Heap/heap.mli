type order

module type Compare =
sig
  type t

  val string : t -> string
  val comp : t * t -> order
end

module type Heap =
sig
  type elem
  type heap

  val empty : unit -> heap
  val isEmpty : heap -> bool
  val insert : elem * heap -> heap
  val deleteMin : heap -> heap
  val findMin : heap -> elem

  val draw : heap -> unit
end

module Int : Compare with type t = int

module Make (El : Compare) : Heap with type elem = El.t
