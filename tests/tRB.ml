let menu (opt) =
  let numItems = Array.length opt-1
  in
    begin
      print_string "\n\n=================================================== \n";
      print_string opt.(0);print_newline();
      for i=1 to numItems do  print_int i; print_string (". "^opt.(i)); print_newline() done;
      print_string "\nSelect an option: ";
      flush stdout;
      let choice = ref (read_int())
      in 
	while !choice < 1 || !choice > numItems do 
	  print_string ("Choose number between 1 and " ^ string_of_int numItems ^ ": ");
	  choice := read_int();
	done; 
	!choice
    end
;;

module A = Rbtree.Make(Rbtree.Int);;

let menuItems = Array.make 6 "";;
let quit = ref false;;
let choice = ref 6;;

let tree = ref (A.empty ());;

menuItems.(0) <- "Tree Operations";
menuItems.(1) <- "insert";
menuItems.(2) <- "delete";
menuItems.(3) <- "isEmpty";
menuItems.(4) <- "member";
menuItems.(5) <- "quit testing";

while not !quit do
  begin
    choice := menu(menuItems);
    match !choice with
	1 ->
	  begin
	    print_string "Set item = ";
	    tree := A.insert (read_int(), !tree);
	    A.draw !tree;
	  end
	    
      | 2 ->
	  begin
	    print_string "Set item = ";
	    tree := A.delete (read_int(), !tree);
	    A.draw !tree;	    
	  end
      | 3 ->
	  print_string ("The tree is " ^ (if (A.isEmpty !tree) then "empty" else "not empty"));
      | 4 ->
	  begin
	    print_string "Set item = ";
	    let i = read_int()
	    in let b = A.member (i, !tree)
	    in print_string ((string_of_int i) ^ " is " ^ (if b then "" else "not ") ^ "a member of the tree")
	  end
      | 5 ->
	  quit := true
      | _ ->
	  print_string "IMPOSSIBLE!!!\n"
  end
done;
Graphics.close_graph ();
