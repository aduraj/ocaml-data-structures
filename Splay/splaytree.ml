open Graphics

type order = Lt | Eq | Gt

module type Compare = 
sig
  type t

  val string : t -> string
  val comp : t * t -> order
end

module type SPLAYTree =
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
  type elem = El.t
  type treeVal = Leaf | Node of treeVal * elem * treeVal
  type tree = treeVal ref

  let empty () = ref Leaf;;

  let isEmpty tree = !tree = Leaf

  let rec splay (x, t) =
    match t with
	Leaf -> Leaf
      | Node (l, y, r) ->
	  (match El.comp(x,y) with
	       Eq -> t
	     | Lt -> 
		 (match l with
		      Leaf -> t
		    | Node (ll, z, rr) ->
			(match El.comp(x,z) with
			     Eq -> Node (ll, z, Node (rr, y, r))
			   | Lt -> 
			       (match ll with
				    Leaf -> Node (ll, z, Node (rr, y, r))
				  | _ -> 
				      let Node (newL, newV, newR) = splay (x, ll)
				      in Node (newL, newV, Node (newR, z, Node (rr, y, r))))
			   | Gt ->
			       (match rr with
				    Leaf -> Node (ll, z, Node (rr, y, r))
				  | _ -> 
				      let Node (newL, newV, newR) = splay (x, rr)
				      in Node (Node (ll, z, newL), newV, Node (newR, y, r)))))
	     | Gt ->
		 (match r with
		      Leaf -> t
		    | Node (ll, z, rr) ->
			(match El.comp(x,z) with
			     Eq -> Node (Node (l, y, ll), z, rr)
			   | Lt ->
			       (match ll with
				    Leaf -> Node (Node (l, y, ll), z, rr)
				  | _ ->
				      let Node (newL, newV, newR) = splay (x, ll)
				      in Node (Node (l, y, newL), newV, Node (newR, z, rr)))
			   | Gt ->
			       (match rr with
				    Leaf -> Node (Node (l, y, ll), z, rr)
				  | _ ->
				      let Node (newL, newV, newR) = splay (x, rr)
				      in Node (Node (Node(l, y, ll), z, newL), newV, newR)))))

  let root t = 
    match t with 
	Node (l, x, r) -> x
      | _ -> failwith "No root of Leaf"

  let member (x, t) =
    match !t with
	Leaf -> false
      | _ ->
	  begin
	    t := splay (x, !t);
	    if El.comp (root !t, x) = Eq
	    then true
	    else false
	  end

  let insert (x, t) =
    match !t with 
	Leaf -> ref (Node (Leaf, x, Leaf))
      | _ -> 
	  let Node (l, y, r) = splay (x, !t)
	  in 
	    (match El.comp (x, y) with
		 Eq -> ref (Node (l, y, r))
	       | Lt -> ref (Node (l, x, Node (Leaf, y, r)))
	       | Gt -> ref (Node (Node (l, y, Leaf), x, r)))

  let delete (x, t) =
    match !t with
	Leaf -> ref Leaf
      | _ ->
	  let Node (l, y, r) = splay (x, !t)
	  in 
	    (match El.comp (x,y) with
		 Eq -> 
		   (match (l,r) with
			(Leaf, _) -> ref r
		      | (_, Leaf) -> ref l
		      | (_, _) ->
			  let Node (newL, newV, newR) = splay (x, l)
			  in ref (Node (newL, newV, r)))
	       | _ -> ref (Node (l, y, r)))

  let rec power x n =
    if n = 0
    then 1
    else
      let a = power x (n / 2) in
	if (n mod 2) = 0
	then a * a
	else x * a * a
      
  let rec depth tree =
    match tree with
	Node (l, _, r) -> max (depth l) (depth r) + 1
      | Leaf -> 0

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

  let draw tree =
    begin
      open_graph "640x480";
      clear_graph ();
      moveto 320 400;
      drawTree (!tree, depth !tree);
    end;;
end
