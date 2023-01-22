(*
                             CS51 Lab 11
                              Synthesis
 *)
(*
                               SOLUTION
 *)

open List ;;

(* Objective:

This lab is intended to provide a review and synthesis of the first
half of the course.
 *)

(*====================================================================
Part 1. Finger exercises

......................................................................
Exercise 1a. Provide a succinct definition of a function named
`reciprocal` that returns the reciprocal of its argument of type
`float`. That is, `reciprocal 2.0` should return `0.5` and `reciprocal
4.0` should return `0.25`. Your implementation may do whatever you
want for an input of `0.`
....................................................................*)

(* The most familiar definition is probably

    let reciprocal x = 1.0 /. x ;;

   or, with full type information,

    let reciprocal (x : float) : float = 1.0 /. x ;;

   Taking advantage of partial application allows for a more succinct
   solution:

    let reciprocal = (/.) 1.0 ;;

   or with type information added: *)

let reciprocal : float -> float = (/.) 1.0 ;;

(*....................................................................
Exercise 1b. Consider a definition of a `person` type as follows.

type person = {first_name: string; last_name: string} ;;

Define an uncurried function named `full_name` that accepts a `string
* string` input representing a person’s first and last name. The
function should return a `person` with that name.
....................................................................*)

type person = {first_name: string; last_name: string} ;;

(* A first implementation might look like

    let full_name (name : string * string) : person =
      let first_name, last_name = name in
      {first_name = first_name; last_name = last_name} ;;

  Taking advantage of field punning allows for:

    let full_name (name : string * string) : person =
      let first_name, last_name = name in
      {first_name; last_name} ;;

  We can also pattern match in the function header, to give us: *)

let full_name (first_name, last_name : string * string) : person =
  {first_name; last_name} ;;

(*....................................................................
Exercise 1c. Define a function named `swap_arguments` that accepts as
input `f`, a curried function that takes two arguments.
`swap_arguments` should return a function that behaves identically to
`f` but swaps the order in which it takes its two arguments.

For example, `(swap_arguments (/)) 2 8` should return `4`, and
`(swap_arguments (<)) 2 8` should return `false`.
....................................................................*)

(* We know that `f` is a curried function that takes two arguments.
   Its type is 'a -> 'b -> 'c. The output of `swap_arguments` should
   therefore have type 'b -> 'a -> 'c since the arguments are reversed.

   We could write `swap_arguments` as:

    let swap_arguments (f : 'a -> 'b -> 'c) : 'b -> 'a -> 'c =
      fun (x : 'b) -> fun (y : 'a) -> f y x ;;

  We can also include the arguments `x` and `y` in the function header
  itself, which results in:
*)

let swap_arguments (f : 'a -> 'b -> 'c) (x : 'b) (y : 'a) : 'c =
  f y x ;;

(*====================================================================
Part 2. Short answer questions

Each of the code snippets below has a blank in it, marked with a long
underline. Your job is to insert a single OCaml pattern or expression
that can fill the blank such that the variable `exercise2x` is defined
to be `42`.

For example, if we provide the snippet

    let f x = x + 3 ;;
    let exercise2n = f _______________ ;;

you could replace the underline with `39` or `3 * 13` or any of a wide
variety of other expressions, like this:

    let f x = x + 3 ;;
    let exercise2n = f 39 ;;

If no such expression exists, replace the entire definition of
`exercise2n` with the integer `0`, that is,

    let exercise2n = 0 ;;

......................................................................
Exercise 2a
....................................................................*)

(* In these solutions, we'll just add a definition of the blank,
   rather than replacing it, so you can see where the blank was.

   In this exercise, `setup` must be a tuple of a value and a
   function that will take that value as an argument. A simple
   solution is to use the identity function: *)

let _______________ = 42, fun x -> x ;;

let exercise2a =
  let setup = _______________ in
  let a, b = setup in
  b a ;;

(*....................................................................
Exercise 2b
....................................................................*)

(* For this expression to evaluate to 42, `setup 0` must raise an
   exception, since otherwise `x` will be `0`. If `setup 0` does raise
   an exception, `x` will be `21`, so `setup 1` must evaluate to `2`.

   A simple solution would therefore be:

    fun x -> if x = 0 then failwith "Failure" else 2 ;;

   A more succinct alternative is also possible:

    fun x -> 2 / x ;;
 *)

let _______________ = fun x -> 2 / x ;;

let exercise2b =
  let setup = _______________ in
  let x = try
            let _ = setup 0
            in 0
          with _ -> 21 in
  x * (setup 1) ;;

(*....................................................................
Exercise 2c
....................................................................*)


(* Of the possible values `1`, `hd * 2`, `hd * 3`, `4`, `hd * 2` or
   `hd * 3` could evaluate to `42`. Therefore, `setup` must be a tuple
   of two lists, one of which is empty. A possible solution is: *)

let _______________ = [], [21] ;;

let exercise2c =
  let setup = _______________ in
  match setup with
  | [], [] -> 1
  | [], hd :: _tl -> hd * 2
  | hd :: _tl, [] -> hd * 3
  | _ -> 4 ;;

(*....................................................................
Exercise 2d
....................................................................*)

(* The sum of `setup 2` and `setup 3` must be `41`. There are multiple
   ways this can happen, but a simple solution is: *)
let _______________ = fun x -> x + 18 ;;

let exercise2d =
  let setup = _______________ in
  1 + setup 2 + setup 3 ;;

(*....................................................................
Exercise 2e
....................................................................*)

(* `setup` is a function that gets applied to three arguments. Since all
   we care about is that the result is `42`, we can ignore the arguments
   and return `42`. *)
let _______________ = fun _ _ _ -> 42 ;;

let exercise2e =
  let setup = _______________ in
  let x = setup 1 in
  let y = x 2 in
  y 3 ;;

(*====================================================================
Part 3. Types of subexpressions in context

Consider this snippet of code, which defines an algebraic data type
`'a merge` and a function `f`:

      type 'a merge =
          | First of ('a -> 'a)
          | Second of ('a * 'a)
      ;;

      let rec f w x y z =
                ^ ---------------------- a
          match w  with
          | [] -> y
          | First a :: b ->
              let c = a x in
              f b c (c :: y) z
                     ^ ----------------- b
          | Second a :: b ->
              let c = f b x in
                      ^^^^^ ------------ c
              c (z a :: y) z
                 ^ --------------------- d
                   ^^^^^^ -------------- e
      ;;

For each of the subexpressions underlined with ^^^ and labeled with a
letter, specify the type of the subexpression in the context in which
it appears. If the underlined element is not a single subexpression (and
therefore has no type), answer "no type".

......................................................................
Exercise 3a: What is the type of the expression `w` that is labeled
"a"? Provide your answer as a string as the value of `exercise2a`:
....................................................................*)

let exercise3a = "'a merge list" ;;

(*....................................................................
Exercise 3b: What is the type of the expression `c` that is labeled
"b"?
....................................................................*)

let exercise3b = "'a" ;;

(*....................................................................
Exercise 3c: What is the type of the expression `f b x` that is
labeled "c"?
....................................................................*)

let exercise3c = "'a list -> ('a * 'a -> 'a) -> 'a list" ;;

(*....................................................................
Exercise 3d: What is the type of the expression `z` that is labeled
"d"?
....................................................................*)

let exercise3d = "'a * 'a -> 'a" ;;

(*....................................................................
Exercise 3e: What is the type of the expression `a :: y` that is
labeled "e"?
....................................................................*)

let exercise3e = "no type" ;;

(*====================================================================
Part 4. Higher-order functional programming

The following problems ask you to define simple functions. Each
solution should nontrivially use one or more of the `List` module
higher-order functions for mapping, folding, and filtering to
implement the function (in addition to constructs from the base OCaml
language). You may make use of other `List` module functions in
addition to your call(s) to `map`, `filter` `fold_left`, or
`fold_right`.

......................................................................
Exercise 4a: An acrostic poem is a poem where taking the nth letter of
each line (typically the first) produces a word. Define a function
`acrostic` that, when given a list of strings and an integer `i`,
returns the string that results from concatenating the character at
index `i` of each string.

For example:

    # acrostic [ "Always"; "Be"; "Closing" ] 0 ;;
    - : string = "ABC"
    # acrostic [ "Accumulating"; "answers"; "of5ends"; "no1" ] 2 ;;
    - : string = "cs51"

Implement `acrostic` using `map`/`fold`/`filter` as specified above.
You may assume the existence of a function `get : string -> int ->
string` such that `get s i` returns a string containing only the
character at index `i` of string `s` (and raises an exception if `s`
does not have at least `i + 1` characters).

Your implementation may do whatever you want if a line is not long
enough. Recall that the binary operator `^` concatenates two strings.
....................................................................*)

let get s i = String.make 1 (String.get s i) ;;

let acrostic (lines: string list) (i: int) : string =
  List.fold_left (fun s line -> s ^ get line i) "" lines ;;

(*....................................................................
Exercise 4b: The `List.for_all` function is described as follows in
the List module documentation:

    val for_all : ('a -> bool) -> 'a list -> bool

`for_all f [a1; ...; an]` checks if all elements of the list satisfy
the predicate `f`. That is, it returns `(f a1) && (f a2) && ... && (f
an)` for a non-empty list and `true` if the list is empty.

It has the following behavior:

    # for_all ((=) 4) [1; 3; 5; 7] ;;
    - : bool = false
    # for_all ((<) 4) [10; 13; 15; 17] ;;
    - : bool = true
    # for_all ((=) 4) [] ;;
    - : bool = true

Re-implement `for_all` using `map`/`fold`/`filter` as specified above.
....................................................................*)

let for_all (f: 'a -> bool) (lst: 'a list) : bool =
  List.fold_left (fun acc x -> acc && f x) true lst ;;

(*====================================================================
Part 5. Cladograms

A cladogram is a diagram that shows the relationship between different
species in evolutionary history.

          ------ Camel
    ------|
          |     ------ Pig
          ------|
                |
                |           ------ Giraffe
                |     ------|
                |     |     ------ Cow
                ------|
                      |     ------ Hippo
                      ------|
                            ------ Whale

For example, according the above cladogram, hippos are more closely
related to whales than they are to giraffes. Cows are more closely
related to pigs than they are to camels (they share a more recent
common ancestor).

A "clade" refers to any group within a cladogram that has a common
ancestor and contains all of that ancestor’s descendants. For example,
giraffes and cows together form a clade (they share a common
ancestor), but cows and hippos together do not form a clade (they
share a common ancestor, but that ancestor also includes giraffes and
whales as descendants).

Consider a definition of a type `clade` as follows: *)

type clade =
  | Species of string
  | Branch of clade * clade
;;

(*....................................................................
Exercise 5a: Define a function `count_species : clade -> int` that
accepts a clade as input and returns the total number of species
contained within the clade. You may assume that any species is
included at most once in any clade.
....................................................................*)

let rec count_species (clade : clade) : int =
  match clade with
  | Species _ -> 1
  | Branch (c1, c2) -> (count_species c1) + (count_species c2) ;;

(*....................................................................
Exercise 5b: Define a function `clade_contains_species : string ->
clade -> bool` that accepts the name of a species and a clade,
returning `true` if the clade contains the species and `false`
otherwise.
....................................................................*)

let rec clade_contains_species (species : string)
                               (clade : clade) : bool =
  match clade with
  | Species s -> s = species
  | Branch (c1, c2) -> (clade_contains_species species c1) ||
                       (clade_contains_species species c2) ;;

(*....................................................................
Exercise 5c: Assume you have access to a function `clade_fold`,
defined below.

    let rec clade_fold (combine : 'a -> 'a -> 'a)
                      (init : string -> 'a)
                      (clade : clade) : 'a =
      match clade with
      | Species s -> init s
      | Branch (c1, c2) -> combine (clade_fold combine init c1)
                                    (clade_fold combine init c2) ;;

Re-implement `count_species` as `count_species_2` with a single call
to `clade_fold`.
....................................................................*)
let rec clade_fold (combine : 'a -> 'a -> 'a)
                    (init : string -> 'a)
                    (clade : clade) : 'a =
  match clade with
  | Species s -> init s
  | Branch (c1, c2) -> combine (clade_fold combine init c1)
                               (clade_fold combine init c2) ;;

let count_species_2 = clade_fold (+) (fun _ -> 1) ;;

(*....................................................................
Exercise 5d: Re-implement `clade_contains_species` as
`clade_contains_species_2` with a single call to the `clade_fold`.
....................................................................*)

let clade_contains_species_2 (s : string) = clade_fold (||) ((=) s) ;;

(*....................................................................
Exercise 5e: Define a function `find_divergence_point : clade ->
string -> string -> clade option`. The function accepts a clade and
two species names, returning the smallest clade that contains both
species (or `None` if no such clade exists).

For example, if we were to represent the example cladogram at the top
of this problem as a value `c`:

  # find_divergence_point c "Cow" "Giraffe" ;;
  - : clade option = Some (Branch (Species "Giraffe", Species "Cow"))
  # find_divergence_point c "Cow" "Llama" ;;
  - : clade option = None

You may make use of functions defined previously in this question.
You may assume that the two species names will be distinct, and you
may assume that any species is included at most once in any clade.
Your solution need not be the most efficient possible: any correct
implementation will receive full credit.
....................................................................*)

(* Since we want the smallest clade possible, we first recursively
   check smaller sub-clades before checking whether the current clade
   contains both species. *)
let rec find_divergence_point (clade : clade)
                              (species1 : string)
                              (species2 : string)
                             : clade option =
  match clade with
  | Species _ -> None
  | Branch (c1, c2) ->
    let c1_div = find_divergence_point c1 species1 species2 in
    let c2_div = find_divergence_point c2 species1 species2 in
    if c1_div <> None then c1_div
    else if c2_div <> None then c2_div
    else if clade_contains_species species1 clade &&
            clade_contains_species species2 clade
    then Some clade
    else None ;;

(* This is not the most efficient solution, as it searches through
   the clade more times than necessary. Making this solution more
   efficient is left as an additional exercise. *)

(*====================================================================
Part 6. A functor for finite sequences

In mathematics, a metric is a function that measures the distance
between two elements from some set. We can define different metrics
for different sets of elements. For example, for the set of integers,
we might define the distance between two integers a and b to be
their absolute difference: |a – b|. Another example is that the
edit distance between two strings can be defined as the number of
characters that need to be added or deleted to make the strings equal.
A metric needs to satisfy certain properties, including that the
distance between any two elements is non-negative, and the distance
between two equal elements is zero.

In this question, we will use signatures and modules allow different
metrics to be defined and use a functor to implement functionality
that will work for any metric.

In the following signature, `METRIC_TYPE`, the function `metric`
computes the distance between elements (or "points") of the abstract
type `t`.
*)

module type METRIC_TYPE =
  sig
    type t
    val metric : t -> t -> float
  end

(*....................................................................
Exercise 6a: Define a module called `FloatMetric` where the type `t`
is equal to `float` and the `metric` is the absolute difference
between two floats. You may use the function `Float.abs` to compute
the absolute value of a float.
...................................................................*)

module FloatMetric =
  struct
    type t = float
    let metric (x : t) (y : t) : float = Float.abs (x -. y)
  end ;;

(*....................................................................
In the rest of this question, we will complete the definition of the
functor `RoutePlanner`, partially defined below, which generates
modules to calculate a route through elements given a particular
metric.

The function `remove ts a`, which returns a list that is identical to
`ts`, except that the element `a` has been removed, has been
implemented for you.

Exercise 6b: Fill in the appropriate type for `t`, the type of an
element in the route.

Exercise 6c: Fill in the implementation of the function `min_distance
a ts`, which returns `None` if `ts` is empty, otherwise it returns
`Some (b, d)`, where `b` is an element of `ts` such that `b` is
closest to `a`, and `d` is the distance between `a` and `b`. That is,
for any element `c` in `ts`, the distance between `a` and `c` is at
least `d`. Do not use any `List` module functions to implement this
function.

Exercise 6d: Fill in the implementation of `route start waypoints`,
which computes a route (i.e., a list of elements of type `t`) that
starts with `start` and goes to all of the elements of `waypoints`.
That is, the length of the list returned by `route` should be one
greater than the length of the list `waypoints`. The algorithm you
must use is greedy: the next element to visit in the route should be
the closest one remaining to be visited.

Your implementation should use `min_distance` and `remove`.
...................................................................*)

module RoutePlanner (MT : METRIC_TYPE) =
  struct
    type t = MT.t

    let rec remove (ts : t list) (a : t) : t list =
      match ts with
      | [] -> []
      | hd :: tl -> if a = hd then tl else hd :: (remove tl a)

    let rec min_distance (a : t) (ts : t list) : (t * float) option =
      match ts with
      | [] -> None
      | hd :: tl ->
        let dist = MT.metric a hd in
        match min_distance a tl with
        | None -> Some (hd, dist)
        | Some (_, d) as m -> if dist < d then Some (hd, dist) else m

    let rec route (start : t) (waypoints : t list) : t list =
      match min_distance start waypoints with
      | None -> [start]
      | Some (a, _) -> start :: (route a (remove waypoints a))
  end

(*....................................................................
Exercise 6e: Write code that defines a module `M` by applying the
functor `RoutePlanner` to the module `FloatMetric`, and then uses the
`route` function to find a route starting at `0.0` and going through
the values `5.1`, `~-.5.0`, `15.2` and `~-.15.3`. Set the resulting
value to `exercise6e`.
...................................................................*)

module M = RoutePlanner(FloatMetric) ;;
let exercise6e = M.route 0. [5.1; ~-.5.0; 15.2; ~-.15.3] ;;

(*....................................................................
Exercise 6f: To ensure that your code above type checks, what
signature should `FloatMetric` have? Fill in your answer into
`exercise6f`.
...................................................................*)

let exercise6f = "METRIC_TYPE with type t = float" ;;

(*
                              END OF LAB
 *)
