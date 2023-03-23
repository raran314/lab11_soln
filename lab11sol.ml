(*
                             CS51 Lab 11
                              Synthesis
 *)
(*
                               SOLUTION
 *)

open List ;;
open CS51Utils.Absbook ;;
  
(* Objective:

This lab is intended to provide a review and synthesis of the first
half of the course.
 *)

(*====================================================================
Part 1. Finger exercises

......................................................................
Each of the expressions below contains a single blank (shown as an
underline "_______"). Your job is to fill in the blank with a single
expression, such that the expression as a whole evaluates to `42`.

If such an expression exists, define the corresponding `answer`
function to return that expression.

If no such expression exists, define the corresponding `answer`
function to raise the `No_such_expression` exception.

For instance, in the example

      (* Exercise 0 

        let x = _______ in
        x + 40 ;;
       *)

      let answer0 () = failwith "not completed" ;;

you would update the definition of the `answer0` function to be 

      let answer0 () = 2 ;;

or

      let answer0 () = 3 - 1 ;;

or any of a wide variety of other expressions that would work.

If you wanted to record that there was no such expression, you'd
instead update the definition of `answer0` to be

      let answer0 () = raise No_such_expression ;;
....................................................................*)

exception No_such_expression ;;

(* Exercise 1.1

      42 / _______ ;;
 *)

let answer1_1 () = 1 ;;
     
(* Exercise 1.2

      let y = _______ in
      let f x = x * x in
      f y + y ;;
 *)

let answer1_2 () = 6 ;;

(* Exercise 1.3

      let x = 6 in 
      let x = 7 in 
      x * ______ ;;
 *)

let answer1_3 () = 6 ;;

(* Exercise 1.4

      List.fold_right (/) [5; 10; 2] _______ ;;
 *)

(* If `x` is an integer filling the blank, the fold computes `5 / (10
   / (2 / x))` but there is no integer `x` such that the integer
   division `5 / x` is 42. *)
let answer1_4 () = raise No_such_expression ;;

(* Exercise 1.5

      List.fold_left (/) _______ [5; 10; 2] ;;
 *)

(* The fold just repeatedly divides the value in the blank by the
   listed numbers. 4200 divided by those numbers is 42. *)
let answer1_5 () = 4200 ;;

(* Exercise 1.6

      let y = _______ in
      let rec f x =
        if x > y then f (x / 2)
        else if x < y then f (succ x)
        else x * x  in
      f 100 ;;
 *)

(* The `f` function always returns the square of `y`. But 42 doesn't
   have an integer square root. *)
let answer1_6 () = raise No_such_expression ;;

(* Exercise 1.7

      (Some 42) |> _______
 *)

(* You could extract the 42 from the option value, e.g., with 

    (fun x -> match x with 
              | Some fortytwo -> 42)

   or even 

    Option.get

   but it's easier just to use a function that always returns 42:

    (fun _ -> 42)
 *)
let answer1_7 () = (fun _ -> 42) ;;

(*====================================================================
Part 2. True / false

Mark each of the propositions below as to whether they are true or
false by defining the corresponding `answer` function to return `true`
or `false`.

 *)

(* Exercise 2.1

   "Uncurried functions provide more opportunity for partial
   application."
 *)

let answer2_1 () = false ;;
  (* Because *curried* functions take their arguments one at a time,
   partial application to some of the arguments is possible. Uncurried
   functions take all of their arguments "at the same time"; partial
   application is not possible. *)

(* Exercise 2.2

   "`None` has a polymorphic type."
 *)

let answer2_2 () = true ;;
(* `None` has the type `'a option`, which is polymorphic because `'a`
   can vary. *)

(* Exercise 2.3

   "All elements of a tuple in OCaml must have the same type."
 *)

let answer2_3 () = false ;;
(* All elements of a list must have the same type. Tuple elements can
   have different types, e.g., `(false, 3)`. *)

(* Exercise 2.4

   "Error values are a good approach to handling anomolous conditions
    when the error value's type encompasses many other values."  *)

let answer2_4 () = false ;;
(* For reasons described in Chapter 10, Section 10.1, error values are
   never a good approach to handling anomalous conditions. *)

(* Exercise 2.5

   "There are no values of type `'a -> 'b`."
 *)

let answer2_5 () = false ;;
(* What about

      (fun _ -> raise Exit)

for instance? *)
  
(*====================================================================
Part 3. Types of subexpressions in context

The next few problems concern a function `raises_exception` that takes
an argument of type `unit -> 'a` and returns a `bool`. It returns
`true` if its argument, when applied to a value of type `unit`,
_raises an exception of any kind_; otherwise, it returns `false`.
 ()
......................................................................
Exercise 3.1: Define the `raises_exception` function. Ideally, your
function should compile without warnings.
....................................................................*)

(* The key to this problem is the use of `try....with` to catch and
   handle the exception. Here's the intended solution: *)
    
let raises_exception (f : unit -> 'a) : bool =
  try let _ = f () in
      false
  with _ -> true ;;

(* Later, in Chapter 15, we'll introduce the `;` operator and the
   `ignore` function. Using `;` we can sequence the application of `f`
   and the `false` return value: 

      let raises_exception (f : unit -> 'a) : bool =
        try f (); 
            false
        with _ -> true ;;

but this may generate a warning that the expression before the
semicolon should have type `unit`. The `ignore` function can help with
that:

      let raises_exception (f : unit -> 'a) : bool =
        try ignore (f ()); 
            false
        with _ -> true ;;
 *)

(*....................................................................
Exercise 3.2: Replace the `failwith` expression below with the
application of `raises_exception` to a single expression returning
`true`.
....................................................................*)

let answer3_2 () = raises_exception (fun () -> raise Exit) ;;

(*....................................................................
Exercise 3.3: Replace the `failwith` expression below with the
application of `raises_exception` to a single expression returning
`false`.
....................................................................*)

let answer3_3 () = raises_exception (fun () -> 42) ;;

(*====================================================================
Part 4. Pascal's triangle

17th century French mathematician Blaise Pascal is well known for his
study of what is popularly known as Pascal's triangle, a (potentially
infinite) grid of numbers that can be indexed by row and column, whose
diagonals represent the binomial coefficients. Here's a depiction of
at least a portion of the triangle

    |  1   2   3   4   5   6   7   8   9  10 ...
-----------------------------------------------
 1  |  1   1   1   1   1   1   1   1   1   1 ...
 2  |  1   2   3   4   5   6   7   8   9  10 ...
 3  |  1   3   6  10  15  21  28  36  45  55 ...
 4  |  1   4  10  20  35  56  84 120 165 220 ...
 5  |  1   5  15  35  70 126 210 330 495 ...
 6  |  1   6  21  56 126 252 462 792 ...
 7  |  1   7  28  84 210 462 924 ...
 8  |  1   8  36 120 330 792 ...
 9  |  1   9  45 165 495 ...
10  |  1  10  55 220 ...
...

All you'll need to know about Pascal's triangle for this problem is this:

  * Each square in the grid contains a number that is the sum of the
    number in the square above it in the grid and the number in the
    square to its left.

  * Squares that have no number above or to the left contain the
    number 1.

......................................................................
Exercise 4.1: Define a function `pascal : int -> int -> int` such that
`pascal row column` returns the number that is in the Pascal's
triangle grid at the designated `row` and `column`. For indices that
are out of bounds, the function should raise an `Invalid_argument`
exception. For instance,

      # pascal 6 5 ;;
      - : int = 126

(You need not concern yourself with the efficiency of your solution.)
....................................................................*)

let rec pascal (row : int) (col : int) : int =
  if row < 1 || col < 1 then
    raise (Invalid_argument "pascal: index out of bounds")
  else if row = 1 || col = 1 then 1
  else pascal (pred row) col + pascal row (pred col) ;;

(*....................................................................
Exercise 4.2: Define a function `test_pascal : unit -> unit` that runs
three substantively distinct unit tests for `pascal` using the
`unit_test` function provided in the `Absbook` module, which you can
assume has already been opened.

For your reference, the documentation for `unit_test` in the `Absbook`
module is duplicated here:

      (* unit_test condition msg -- Tests `condition` and prints an
         indicative message `msg` related to the condition along with
         a passed or failed string *)

....................................................................*)

(* Here are four substantively distinct tests: *)
let test_pascal () =
  unit_test (pascal 1 5 = 1) "first row";
  unit_test (pascal 5 1 = 1) "first column";
  unit_test (pascal 6 5 = 126) "interior";
  unit_test (try let _ = pascal 6 0 in
                 false
             with
             | Invalid_argument _ -> true
             | _ -> false) "out of bounds" ;;

(*====================================================================
Part 5. Combo values

The problems in this section concern a `('left, 'right) combo`
algebraic data type, defined below, which can be used to combine
values of the two types `'left` and `'right`, up to one value of
each. *)

type ('left, 'right) combo =
  | Neither
  | Left of 'left
  | Right of 'right
  | Both of 'left * 'right ;;

(* (Combos are a bit like pairs, since they combine two values of
particular types, except that the left and right values are in a way
optional, that is, you can leave either one or both off.) *)

(*....................................................................
Exercise 5.1: Define a function named `combo_example` that takes a
unit argument and returns a value of this type that combines the value
`true` and the value `3.14` as left and right components.
....................................................................*)

let combo_example () =
  Both (true, 3.14) ;;

(*....................................................................
Exercise 5.2: What is the return type of the `combo_example` function?
You can check your answer at <https://url.cs51.io/lab11-2023-1>.
....................................................................*)

(*....................................................................
Exercise 5.3: Define a function `left_list` that takes a list of
values and returns a list of combos with each value used as the *left*
value in a combo. For instance,

      # left_list [3; 4; 5] ;;
      - : (int, 'a) combo list = [Left 3; Left 4; Left 5]
      # left_list [] ;;
      - : ('a, 'b) combo list = []

Ideally, your function definition will be quite succinct; just a
couple of lines should be sufficient.
....................................................................*)
let left_list (lefts : 'left list) : ('left, 'right) combo list =
  List.map (fun x -> Left x) lefts ;;

(* You might have tried using partial application here: 

      let left_list : 'left list -> ('left, 'right) combo list =
        List.map (fun x -> Left x) ;;    

   Unfortunately, that partial application is too difficuly for OCaml
   to refer types on, so it resorts to weak type variables:

      # let left_list : 'left list -> ('left, 'right) combo list =
        List.map (fun x -> Left x) ;;    
      val left_list : '_left list -> ('_left, 'right) combo list = <fun>

   Happily that's not the kind of thing we'd take off for, on a
   midterm, say.  *)

(* In the remaining problems in this section, you can assume that the
function `left_list` is available, and if you provide `right_list` you
can use that too. *)
       
(* For completeness, here's `right_list`. *)

let right_list (rights : 'right list) : ('left, 'right) combo list =
  List.map (fun x -> Right x) rights ;;
     
(*....................................................................
Exercise 5.4: The `combo` type is useful in situations where you want
to pair values of particular types but sometimes one of the two values
is not available.

For example, recall the `zip` function from the textbook and the lab,
a version of which is duplicated here for your convenience. It raises
an exception on lists of unequal lengths.

      let rec zip (lefts : 'left list) (rights : 'right list) =
        match lefts, rights with
        | [], [] -> []
        | [], _
        | _, [] -> raise (Failure "zip: lists of unequal lengths")
        | left_hd :: left_tail, right_hd :: right_tl ->
           (left_hd, right_hd) :: (zip left_tail right_tl) ;;

We've left off the return value type from the header line of
`zip`. What is the type of the return value of `zip` as defined above?
You can verify your answer at <https://url.cs51.io/lab11-2023-2>.
....................................................................*)

(*....................................................................
Exercise 5.5: Now consider a function `zip_combo` that works much like
`zip` except that it returns a list of `combo` values instead of a
list of pairs. For lists of unequal lengths, we can use the extra
possibilities in `combo` values to retain the unpaired left or right
elements, so `zip_combo` needn't raise an exception in such cases. For
instance,

      # zip_combo [1; 2; 3] [true; false; true] ;;
      - : (int, bool) combo list =
        [Both (1, true); Both (2, false); Both (3, true)]

      # zip_combo [1; 2; 3] [true; false] ;;
      - : (int, bool) combo list =
        [Both (1, true); Both (2, false); Left 3]

      # zip_combo [1; 2] [true; false; true] ;;
      - : (int, bool) combo list =
        [Both (1, true); Both (2, false); Right true]

What is the type of `zip_combo`? You can verify your answer at
<https://url.cs51.io/lab11-2023-3>.
....................................................................*)

(*....................................................................
Exercise 5.6: Define the `zip_combo` function that works in this way.
....................................................................*)

let rec zip_combo (lefts : 'left list)
                  (rights : 'right list)
                : ('left, 'right) combo list =
  match lefts, rights with
  | [], rights -> right_list rights
  | lefts, [] -> left_list lefts
  | left_hd :: left_tl, right_hd :: right_tl ->
     Both (left_hd, right_hd) :: (zip_combo left_tl right_tl) ;;

(*====================================================================
Part 6. Directory structures

Files on a computer are typically organized into a directory
structure, a tree-like structure of *nodes* which can be either
directories or files. (For Mac users, directories are typically
referred to as "folders".) Directories store zero or more nodes, and
files store *contents* of some sort. In this section, you'll work with
a functor for directory structures.

A simple signature for a directory structure module is as follows: *)

module type DIRECTORY =
  sig
    (* The type of file contents *)
    type contents
    (* The type of nodes, both directories and files *)
    type node

    (* new_dir str contents -- Returns a new directory node named
       `str`. *)
    val new_dir : string -> node
    (* new_file str contents -- Returns a new file node named `str`
       holding the given `contents`. *)
    val new_file : string -> contents -> node
    (* is_dir node -- Returns `true` just in case `node` is a
       directory node. *)
    val is_dir : node -> bool
    (* add_node new_node dir -- Returns a directory node that augments
       directory node `dir` with an additional node `new_node`. Raises 
       an `Invalid_argument` exception if `dir` is not a directory 
       node. *)
    val add_node : node -> node -> node
    (* node_at start path -- Returns the node within `start` at the
       given `path`, which is a list of node names, as an option
       value.  Raises `Not_found` if no such node exists. *)
    val node_at : node -> string list -> node
    (* contents_of node -- Returns a string representation of the
       contents of the `node` if a file node; or if a directory,
       returns the name of the directory. *)
    val contents_of : node -> string
    (* finds_path target_name node -- Returns a path, a list of names
       of nodes, that constitute the path to a node named `target`
       within the given `node`. The return value is `None` if no such
       node exists within `node`, and `Some ...path...` otherwise.*)
    val find_path : string -> node -> string list option
  end ;;

(* Directory structures might differ as to what the contents of files
are. We'll make this possible with a signature for a module that
specifies what the file contents are, including a type `t` for file
contents and a function `string_of` that returns a string
representation of a file's contents. *)
  
module type FILE_CONTENTS =
  sig
    (* The type of the contents of a file. *)
    type t
    (* string_of contents -- Returns a string representation of the
       `contents`.*)
    val string_of : t -> string
  end ;;

(* Finally, here's a (partial) implementation of a functor called
`MakeDirectory`. The functor should take as an argument a module named
`Contents` satisfying an appropriate file contents signature and
return a module that satisfies an appropriate directory structure
signature.

We've commented this out because we left off a key component of the
functor definition – the rest of the functor's header. *)

(*....................................................................
Exercise 6.1: What should go in the blank to complete the header so
that the functor definition is *complete and usable*? Remember that
the functor should take as an argument a module named `Contents`
satisfying an appropriate file contents signature and return a module
that satisfies an appropriate directory structure signature.

After you've filled in the blank, uncomment the definition of the
`MakeDirectory` functor so you can use it below.
...................................................................*)

module MakeDirectory (Contents : FILE_CONTENTS)
       : (DIRECTORY with type contents = Contents.t) =
  struct
    
    type contents = Contents.t
      
    type node =
      | Directory of string * node list
      | File of string * contents
                           
    let is_dir (node : node) : bool =
      match node with
      | File _ -> false
      | Directory _ -> true
                         
    let new_dir (name : string) : node =
      Directory (name, [])
                
    let new_file (name : string) (contents : contents) : node =
      File (name, contents)
           
    let name_of (node : node) : string =
      match node with
      | File (name, _)
      | Directory (name, _) -> name
                                        
    let add_node (node : node) (in_dir : node) : node =
      match in_dir with
      | File _ -> raise (Invalid_argument "can't add file to a non-directory")
      | Directory (dir_name, nodes) ->
         Directory (dir_name, node :: nodes)
                   
    let node_at (root : node) (path : string list) : node =
      
      let node_named target nodes =
        List.find (fun node -> name_of node = target) nodes in
      
      let rec node_at' roots path =
        match path with
        | [] -> raise (Failure "node_at: empty path")
        | name :: tl ->
           let next = node_named name roots in
           if tl = [] then next
           else
             (match next with
              | Directory (_name, subnodes) -> node_at' subnodes tl
              | File _ -> raise (Failure "node_at: no such path")) in

      node_at' [root] path
                    
    let contents_of (node : node) : string =
      match node with
      | File (_name, contents) -> Contents.string_of contents
      | Directory (name, _subnodes) -> name
                                         
    let first_some lst =
      List.nth_opt (List.concat (List.map Option.to_list lst)) 0
                   
    let rec find_path (target : string) (node : node) : string list option =
      match node with
      | File (name, _contents) ->
         if target = name then Some [name]
         else None
      | Directory (name, nodes) ->
         if target = name then Some [name]
         else
           let subpath = first_some (List.map (find_path target) nodes) in
           match subpath with
           | None -> None
           | Some subpath_val -> Some (name :: subpath_val)
  end ;;
     
(*....................................................................
Exercise 6.2: Let's suppose we wanted to set up a directory structure
module where the contents of a file could be either an integer or a
string or both or neither.

Define a module called `IntOrStringDirectory` that implements a
directory structure of that sort. Make sure to restrict the module to
an appropriate type signature.

You'll want to make use of the `combo` type from problem 6.
...................................................................*)

(* The `MakeDirectory` functor can be applied to an anonymous module
   specifying the contents, to generate the `IntOrStringDirectory`
   module:

module IntOrStringDirectory
       : (DIRECTORY with type contents = (int, string) combo) =
  MakeDirectory (struct
                  type t = (int, string) combo
                  let string_of (contents : t) : string =
                    match contents with
                    | Neither -> "no contents"
                    | Left n -> string_of_int n
                    | Right s -> s
                    | Both (n, s) -> (string_of_int) n ^ " " ^ s
                end) ;;
 *)

(* Alternatively, we can name the contents specification module, and
   then apply the functor: *)
  
module IntOrStringContents : (FILE_CONTENTS with type t = (int, string) combo) =
  struct
    type t = (int, string) combo
    let string_of (contents : t) : string =
      match contents with
      | Neither -> "no contents"
      | Left n -> string_of_int n
      | Right s -> s
      | Both (n, s) -> (string_of_int) n ^ " " ^ s
  end ;;
  
module IntOrStringDirectory
       : (DIRECTORY with type contents = (int, string) combo) =
  MakeDirectory (IntOrStringContents) ;;

(*....................................................................
Exercise 6.3: Define a function `example` of type `unit ->
IntOrStringDirectory.node` that returns a directory named `"root"`
that contains two files, named `"file1"` and `"file2"`, each of which
has different contents of your choice.
...................................................................*)

(* In this example, we actually put in four files so as to try out the
   different contents options. *)
let example () : IntOrStringDirectory.node =
  let open IntOrStringDirectory in
  new_dir "root"
  |> add_node (new_file "file1" (Left 3))
  |> add_node (new_file "file2" (Right "hello"))
  |> add_node (new_file "file3" (Both (4, "four")))
  |> add_node (new_file "file4" Neither) ;;

(* To test that the example is working, we define a function that can
   extract a string representation of a file with a given name: *)

let contents_of_name node name =
  let open IntOrStringDirectory in
  node
  |> find_path name
  |> Option.get
  |> node_at node
  |> contents_of ;;

(* Testing this on the four files in `example`:

      # contents_of_name example "file1" ;;
      - : string = "3"
      # contents_of_name example "file2" ;;
      - : string = "hello"
      # contents_of_name example "file3" ;;
      - : string = "4 four"
      # contents_of_name example "file4" ;;
      - : string = "no contents"
 *)
   
(*
                              END OF LAB
 *)
