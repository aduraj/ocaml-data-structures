open Graphics

type order = Lt | Eq | Gt

module type Compare = 
sig
  type t

  val string : t -> string
  val comp : t * t -> order
end

module type AVLTree =
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
  type tree = Leaf | Node of int * tree * elem * tree

  let empty () = Leaf

  let isEmpty tree = tree = Leaf

  let rec member (e, t) = 
    match t with
	Node (_,l,x,r) ->
	  (match El.comp (e, x) with
	       Lt -> member(e, l)
	     | Eq -> true
	     | Gt -> member (e, r))
      | Leaf -> false

  let depth tree = 
    match tree with
	Node (d, _, _, _) -> d
      | Leaf -> 0

  let value tree = 
    match tree with
	Node (_, _, x, _) -> x
      | Leaf -> failwith "Impossible"

  let balanceLL node = 
    match node with 
	(Node (d, Node (lmax, ll, xl, rl), x, r)) ->
	  let rmax = max (depth rl) (depth r) + 1 
	  in let cmax = max rmax (depth ll) + 1
	  in Node (cmax, ll, xl, Node (rmax, rl, x, r))
      | _ -> failwith "Impossible"

  let balanceLR node =
    match node with 
	(Node (d, Node (dl, ll, y, Node (dlr, lrl, z, lrr)), x, r)) -> 
	  let lmax = max (depth ll) (depth lrl) + 1
	  in let rmax = max (depth lrr) (depth r) + 1
	  in let cmax = max lmax rmax + 1
	  in Node (cmax, Node (lmax, ll, y, lrl), z, Node (rmax, lrr, x, r))
      | _ -> failwith "Impossible"

  let balanceRR node = match node with
      (Node (d, l, x, Node (dr, lr, xr, rr))) ->
	let lmax = max (depth l) (depth lr) + 1
	in let cmax = max lmax (depth rr) + 1
	in Node (cmax, Node (lmax, l, x, lr), xr, rr)
    | _ -> failwith "Impossible"

  let balanceRL node =
    match node with 
	(Node (d, l, x, Node (dr, Node (drl, rll, z, rlr), y, rr))) ->
	  let lmax = max (depth l) (depth rll) + 1
	  in let rmax = max (depth rlr) (depth rr) + 1
	  in let cmax = max lmax rmax + 1
	  in Node (cmax, Node (lmax, l, x, rll), z, Node (rmax, rlr, y, rr)) 
      | _ -> failwith "Impossible"

  let rec insert (e, t) = 
    match t with
	Node (_, l, x, r) ->
	  (match El.comp (e, x) with
	       Lt -> 
		 let insL = insert (e, l)
		 in let dl = depth insL
		 in let dr = depth r
		 in let bal = dl - dr
		 in
		   if bal <> 2
		   then Node ((max dr dl) + 1, insL, x, r)
		   else if e < value l
		   then balanceLL (Node (dl + 1, insL, x, r))
		   else if e > value l
		   then balanceLR (Node (dl + 1, insL, x, r))
		   else t
	     | Eq -> t
	     | Gt -> 
		 let insR = insert (e, r)
		 in let dr = depth insR
		 in let dl = depth l
		 in let bal = dl - dr
		 in
		   if bal <> -2
		   then Node ((max dr dl) + 1, l, x, insR)
		   else if e > value r
		   then balanceRR (Node (dr + 1, l, x, insR))
		   else if e < value r
		   then balanceRL (Node (dr + 1, l, x, insR))
		   else t)
      | Leaf -> Node (1, Leaf, e, Leaf)

  let rec min tree = 
    match tree with
	Node (_, Leaf, x, _) -> x
      | Node (_, l, _, _) -> min l
      | Leaf _ -> failwith "Impossible"
  
  let left tree = 
    match tree with
	Node (_, l, _, _) -> l
      | Leaf -> failwith "Impossible"  

  let rigth tree =
    match tree with
	Node (_, _, _, r) -> r
      | Leaf -> failwith "Impossible"

  let rec delete (e, t) = 
    match t with
	Node (_, l, x, r) ->
	  (match El.comp (e, x) with
	       Eq -> 
		 (match (l, r) with
		      (Leaf, Leaf) -> Leaf
		    | (Leaf, _) -> r
		    | (_, Leaf) -> l
		    | (_, _) -> 
			let m = min r
			in let del = delete (m, r)
			in let bal = depth l - depth del
			in if bal < 2
			  then Node ((max (depth l) (depth del)) + 1, l, m, del)
			  else 
			    let balL = depth (left l) - depth (rigth l)
			    in if balL > 0
			      then balanceLL (Node (((max (depth l) (depth del)) + 1, l, m, del)))
			      else balanceLR (Node (((max (depth l) (depth del)) + 1, l, m, del))))
	     | Lt ->  
		 let delL = delete (e, l)
		 in let dl = depth delL
		 in let dr = depth r
		 in let bal = dl - dr
		 in
		   if abs bal < 2
		   then Node ((max dl dr) + 1, delL, x, r)
		   else 
		     let balR = depth (left r) - depth (rigth r)
		     in if balR < 0
		       then balanceRR (Node ((max dl dr) + 1, delL, x, r))
		       else balanceRL (Node ((max dl dr) + 1, delL, x, r))
	     | Gt ->
		 let delR = delete (e, r)
		 in let dl = depth l
		 in let dr = depth delR
		 in let bal = dl - dr
		 in
		   if abs bal < 2
		   then Node ((max dl dr) + 1, l, x, delR)
		   else
		     let balL = depth (left l) - depth (rigth l)
		     in if balL > 0
		       then balanceLL (Node ((max dl dr) + 1, l, x, delR))
		       else balanceLR (Node ((max dl dr) + 1, l, x, delR)))
      | Leaf -> Leaf

  let rec power x n =
    if n = 0
    then 1
    else
      let a = power x (n / 2) in
	if (n mod 2) = 0
	then a * a
	else x * a * a
      
  let rec drawTree tree =
    let (x, y) = (current_x (), current_y ())
    in let a = (depth tree) - 2 
    in let b = (power 2 a) * 10
    in
      match tree with
	  Leaf  -> ()
	| Node (d, Leaf, v, Leaf) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y
	    end 
	| Node (d, l, v, Leaf) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x - 10) (y - 20);
	      drawTree l;
	    end
	| Node (d, Leaf, v, r) ->
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x + 10) (y - 20);
	      drawTree r;
	    end
	| Node (d, l, v, r) -> 
	    begin
	      draw_circle x y 5;
	      set_color blue;
	      draw_string (El.string v);
	      set_color black;
	      moveto x y;
	      lineto (x - b) (y - 20);
	      drawTree l;
	      moveto x y;
	      lineto (x + b) (y - 20);
	      drawTree r;
	    end;;

  let draw tree =
    begin
      open_graph "640x480";
      clear_graph ();
      moveto 320 400;
      drawTree tree;
    end;;
end
