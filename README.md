<h3>Run</h3> 
* ocaml -c library.mli
* ocaml -c library.ml

in order to get library.cmi and library.cmo

<hr>

Copy the generated files (cmi and cmo) to the lib directory inside your ocaml installation folder. The library is now available to all your ocaml apps. The library name is the capitalized file name: library.cmo -> Library.

In order to use a library just in a single application, copy the generated files to this application's directory.

<hr>

In order to compile program.ml which uses library.cmo, run:
* ocamlc graphics.cma library.cmo program.ml -o program
    
The Graphics library is required for all the libraries in this repository.

<hr>

In order to load one of the libraries in the interactive mode, run:
* #load "graphics.cma";;
* #load "library.cmo";;

<hr>

Each library provides module Int of signature Compare and functor Make(El:Compare) of signature appropriate for the library (e.g. Avltree).

<hr>

Sample usage:
* module M = Avltree.Make(Avltree.Int);;
* let tree = M.empty();;

<hr>

Methods for tree libraries:
* empty : unit -> tree 
* insert : elem * tree -> tree 
* delete : elem * tree -> tree 
* member : elem * tree -> bool 
* isEmpty : tree -> bool 
* draw : tree -> unit 

<hr>

Methods for heap library:
* empty : unit -> heap 
* isEmpty : heap -> bool 
* insert : elem * heap -> heap 
* findMin : heap -> elem 
* Draw : heap -> unit 

<hr>

There are sample programs in the tests directory (.ml + executables).

<hr>

In order to run a program, type in shell:
* ocamlrun executable
    