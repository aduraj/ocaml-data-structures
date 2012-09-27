(* Red-black tree *)

open Graphics

type order = Lt | Eq | Gt

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
  type color = R | B | BB
  type elem = El.t
  type tree = Leaf of color | Node of color * tree * elem * tree

  let empty () = Leaf B

  let isEmpty tree = tree = (Leaf B)

  let rec member (e, t) = 
    match t with
	Node (_,l,x,r) ->
	  (match El.comp (e, x) with
	       Lt -> member(e, l)
	     | Eq -> true
	     | Gt -> member (e, r))
      | Leaf _ -> false

  let insBalL (c, l, x, r) = 
    match (c, l) with
	(B, Node (R, Node (R, s, y, t), z, u)) -> Node (R, Node (B, s, y, t), z, Node (B, u, x, r))
      | (B, Node (R, s, z, Node (R, t, y, u))) -> Node (R, Node (B, s, z, t), y, Node (B, u, x, r))
      | _ -> Node (c, l, x, r)

  let insBalR (c, l, x, r) = 
    match (c, r) with
	(B, Node (R, s, y, Node (R, t, z, u))) -> Node (R, Node (B, l, x, s), y, Node (B, t, z, u))
      | (B, Node (R, Node (R, s, z, t), y, u)) -> Node (R, Node (B, l, x, s), z, Node (B, t, y, u))
      | _ -> Node (c, l, x, r)

  let insert (e, t) = 
    let rec aux tree = 
      match tree with
	  Node (c, l, x, r) ->
	    (match El.comp (e, x) with
		 Lt -> insBalL (c, aux l, x, r)
	       | Gt -> insBalR (c, l, x, aux r)
	       | Eq -> tree)
	| Leaf _ -> Node (R, Leaf B, e, Leaf B)
    in let node = aux t
    in match node with
	Node (_, left, el, rigth) -> Node (B, left, el, rigth)
      | Leaf _ -> failwith "Impossible";;

  let rec min tree = 
    match tree with
	Node (_, Leaf _, x, _) -> x
      | Node (_, l, _, _) -> min l
      | Leaf _ -> failwith "Impossible"
	  
  let unBB tree = 
    match tree with
	Leaf BB -> Leaf B
      | Node (BB, l, x, r) -> Node (B, l, x, r)
      | _ -> failwith "Impossible"

  let addB tree = 
    match tree with
	Node (R, l, x, r) -> Node (B, l, x, r)
      | Node (B, l, x, r) -> Node (BB, l, x, r)
      | Leaf B -> Leaf BB
      | _ -> failwith "Impossible"

  let value tree = 
    match tree with
	Node (_, _, x, _) -> x
      | Leaf _ -> failwith "Impossible"
	  
  let left tree = 
    match tree with
	Node (_, l, _, _) -> l
      | Leaf _ -> failwith "Impossible"  

  let rigth tree =
    match tree with
	Node (_, _, _, r) -> r
      | Leaf _ -> failwith "Impossible"
	  
  let isBlack tree = 
    match tree with
	Leaf B -> true
      | Node (B, _, _, _) -> true
      | _ -> false

  let isRed tree = 
    match tree with
	Node (R, _, _, _) -> true
      | _ -> false 

  let double tree = 
    match tree with
	Node (BB, _, _, _) -> true
      | Leaf BB -> true
      | _ -> false

  let rec balDelL node = 
    match node with
	(B, d, y, Node (R, l, z, r)) ->
	  if double d
	  then Node (B, balDelL (R, d, y, l), z, r)
	  else Node (B, d, y, Node (R, l, z, r))
      | (c, d, y, Node (B, l, z, r)) ->
	  if double d
	  then 
	    if isBlack l && isBlack r
	    then addB (Node (c, unBB d, y, Node (R, l, z, r)))
	    else if isRed l && isBlack r
	    then balDelL (c, d, y, Node (B, left l, value l, Node (R, rigth l, z, r)))
	    else Node (c, Node (B, unBB d, y, l), z, addB r)
	  else Node (c, d, y, Node (B, l, z, r))
      | (c, l, x, r) -> Node (c, l, x, r)

  let rec balDelR node = 
    match node with
	(B, Node (R, l, z, r), y, d) ->
	  if double d
	  then Node (B, l, z, balDelR (R, r, y, d))
	  else Node (B, Node (R, l, z, r), y, d)
      | (c, Node (B, l, z, r), y, d) ->
	  if double d
	  then
	    if isBlack l && isBlack r
	    then addB (Node (c, Node (R, l, z, r), y, unBB d))
	    else if isBlack l && isRed r
	    then balDelR (c, Node (B, Node (R, l, z, left r), value r, rigth r), y, d)
	    else Node (c, addB l, z, Node (B, r, y, unBB d))
	  else Node (c, Node (B, l, z, r), y, d)
      | (c, l, x, r) -> Node (c, l, x, r)

  let rec del (e, t) = 
    let rec aux tree = 
      match tree with
	  Node (R, Leaf _, x, Leaf _) ->
	    if El.comp (e, x) = Eq then Leaf B else tree
	| Node (B, Leaf _, x, Leaf _) ->
	    if El.comp (e, x) = Eq then Leaf BB else tree
	| Node (_, Leaf _, x, Node (_, l, y, r)) ->
	    if El.comp (e, x) = Eq 
	    then Node (B, l, y, r)
	    else if El.comp (e, y) = Eq
	    then Node (B, Leaf B, x, Leaf B)
	    else tree
	| Node (_, Node (_, l, y, r), x, Leaf _) ->
	    if El.comp (e, x) = Eq
	    then Node (B, l, y, r)
	    else if El.comp (e, y) = Eq
	    then Node (B, Leaf B, x, Leaf B)
	    else tree 
	| Node (c, l, x, r) ->
	    (match El.comp (e, x) with
		 Lt -> balDelL (c, aux l, x, r)
	       | Gt -> balDelR (c, l, x, aux r)
	       | Eq -> 
		   let m = min r
		   in balDelR (c, l, m, del (m, r)))
	| Leaf _ -> tree
    in
      aux t  
	
  let delete (e, t) = 
    match del (e, t) with
	Node (_, l, x, r) -> Node (B, l, x, r)
      | Leaf _ -> Leaf B

  let setCol c = 
    match c with
	R -> set_color red
      | _ -> set_color black

  let rec depth tree =
    match tree with
	Node (_, l, _, r) -> max (depth l) (depth r) + 1
      | Leaf _ -> 0 

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
    in let b = (power 2 a) * 15
    in
      match tree with
	  Leaf _ -> ()
	| Node (c, Leaf _, v, Leaf _) ->
	    begin
	      setCol c;
	      fill_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	    end
	| Node (c, l, v, Leaf _) ->
	    begin
	      setCol c;
	      fill_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x - 10) (y - 20);
	      drawTree (l,d-1);
	    end
	| Node (c, Leaf _, v, r) ->
	    begin
	      setCol c;
	      fill_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x + 10) (y - 20);
	      drawTree (r,d-1);
	    end
	| Node (c, l, v, r) -> 	    
	    begin
	      setCol c;
	      fill_circle x y 5;
	      set_color black;
	      draw_string (El.string v);
	      moveto x y;
	      lineto (x - b) (y - 20);
	      drawTree (l,d-1);
	      moveto x y;
	      lineto (x + b) (y - 20);
	      drawTree (r,d-1);
	    end;;

  let draw tree =
    begin
      open_graph "640x480";
      clear_graph ();
      moveto 320 400;
      drawTree (tree, depth tree);
    end;;

end
