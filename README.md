1. Run 
    ocaml -c library.mli
    ocaml -c library.ml

in order to get library.cmi and library.cmo

2. Copy the generated files (cmi and cmo) to the lib directory inside your ocaml installation folder. The library is now available to all your ocaml apps. The library name is the capitalized file name: library.cmo -> Library.
In order to use a library just in a single application, copy the generated files to this application's directory.

3. In order to compile program.ml which uses library.cmo, run:
    ocamlc graphics.cma library.cmo program.ml -o program
    
The Graphics library is required for all the libraries in this repository.

4. In order to load one of the libraries in the interactive mode, run:
    #load "graphics.cma";;
    #load "library.cmo";;
    
5. Each library provides module Int of signature Compare and functor Make(El:Compare) of signature appropriate for the library (e.g. Avltree).
    
6. Sample usage:
    module M = Avltree.Make(Avltree.Int);;
    let tree = M.empty();;
    
7. Methods for tree libraries:
    a. empty : unit -> tree 
    b. insert : elem * tree -> tree 
    c. delete : elem * tree -> tree 
    d. member : elem * tree -> bool 
    e. isEmpty : tree -> bool 
    f. draw : tree -> unit 
    
8. Methods for heap library:
    a. empty : unit -> heap 
    b. isEmpty : heap -> bool 
    c. insert : elem * heap -> heap 
    e. findMin : heap -> elem 
    f. Draw : heap -> unit 

9. There are sample programs in the tests directory (.ml + executables).

10. In order to run a program, type in shell:
    ocamlrun executable
    