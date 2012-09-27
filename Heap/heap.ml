open Graphics

type order = Lt | Eq | Gt

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

module Int =
struct
  type t = int

  let string x = string_of_int x

  let comp (x, y) = 
    if x > y 
    then Gt
    else if x < y
    then Lt
    else Eq
end

module Make (El : Compare) =
struct
  type elem = El.t
  type tree = Leaf | Node of tree * elem * tree
  type heap = H of int * tree

  let empty () = H (0, Leaf)

  let isEmpty (H (n, _)) = n = 0

  let rec ins (n, x, t) =
    match t with
	Leaf -> Node (Leaf, x, Leaf)
      | Node (l, y, r) ->
	  if n mod 2 = 0
	  then 
	    let (Node (newL, newV, newR) as newT) = ins (n/2, x, l)
	    in 
	      if newV < y
	      then  Node (Node (newL, y, newR), newV, r)
	      else Node (newT, y, r)
	  else 
	    let (Node (newL, newV, newR) as newT) = ins (n/2, x, r)
	    in 
	      if newV < y
	      then  Node (l, newV, Node (newL, y, newR))
	      else Node (l, y, newT)

  let insert (x, H (n, t)) = H (n + 1, ins (n+1, x, t))

  let findMin (H (_, t)) =
    match t with
	Node (_, x, _) -> x
      | Leaf -> failwith "No elements in the heap"

  let rec check (Node (l, x, r) as t) =
    match (l,r) with
	(Leaf,Leaf) -> t
      | (Leaf, Node (rl, xr, rr)) ->
	  if x > xr
	  then Node (l, xr, Node (rl, x, rr))
	  else t
      | (Node (ll, xl, lr), Leaf) -> 
	  if x > xl
	  then Node (Node (ll, x, lr), xl, r)
	  else t
      | (Node (ll, xl, lr), Node (rl, xr, rr)) ->
	  if x <= xl && x <= xr
	  then t
	  else if xl < xr
	  then Node (check (Node (ll, x, lr)), xl, r)
	  else Node (l, xr, check (Node (rl, x, rr)))

  let rec del (n, Node (l, x, r)) =
    match (l, r) with
	(Leaf, Leaf) -> (x, Leaf)
      | _ ->
	  if n mod 2 = 0
	  then 
	    let (newV, newL) = del (n/2, l)
	    in (x, check (Node (newL, newV, r)))
	  else
	    let (newV, newR) = del (n/2, r)
	    in (x, check (Node (l, newV, newR)))

  let deleteMin (H(n, t)) =
    match t with
	Leaf -> H (0, Leaf)
      | _ -> 
	  let (newV, newT) = del (n, t)
	  in H (n-1, newT)

  let rec depth tree =
    match tree with
	Node (l, _, r) -> max (depth l) (depth r) + 1
      | Leaf -> 0

  let rec power x n =
    if n = 0
    then 1
    else
      let a = power x (n / 2) in
	if (n mod 2) = 0
	then a * a
	else x * a * a

  let rec drawTree (tree, d) =
    let (x, y) = (current_x (), current_y ())
    in let a = d - 2 
    in let b = (power 2 a) * 10
    in
      match tree with
	  Leaf  -> ()
	| Node (Leaf, v, Leaf) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y
	    end 
	| Node (l, v, Leaf) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x - 10) (y - 20);
	      drawTree (l, d-1);
	    end
	| Node (Leaf, v, r) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x + 10) (y - 20);
	      drawTree (r, d-1);
	    end
	| Node (l, v, r) -> 
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x - b) (y - 20);
	      drawTree (l,d-1);
	      moveto x y;
	      lineto (x + b) (y - 20);
	      drawTree (r,d-1);
	    end;;

  let draw (H(_, t)) =
    begin
      open_graph "640x480";
      clear_graph ();
      moveto 320 400;
      drawTree (t, depth t);
    end;;
end
