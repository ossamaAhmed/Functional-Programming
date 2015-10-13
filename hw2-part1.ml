(*
 * Lucien GEORGE: 260571775
 * Ossama AHMED: 260549558
 *)

(* HOMEWORK 1 : COMP 302 Fall 2015

   
   PLEASE NOTE:  

   * All code files must be submitted electronically
     BEFORE class on 8 Oct 2015

  *  The submitted file name must be hw2-part1.ml 

  *  Submitted solutions will be graded according to their correctness and elegance. 
     Please consult the OCaml style guide posted on the course website.

  *  Your program must type-check and run usig OCaml of at least OCaml 4.0

  * Remove all "raise NotImplemented" with your solutions
*)

exception NotImplemented
exception Error

(* -------------------------------------------------------------*)
(* QUESTION 1 :  Proofs  [25 points]                            *) 
(* -------------------------------------------------------------*)

let rec concat l = match l with 
  | [] -> ""
  | x::xs ->  x ^ (concat xs)

let concat' l = 
let rec conc l acc = match l with
  | [] -> acc
  | x::xs -> conc xs (acc ^ x)
in 
   conc l ""


(* Your friend implements the following version of concat;
   he claims it is a better function, as it is tail-recursive.

  A. Is he right? - Discuss the run-time behaviour between these two functions (5 points)

  B. Prove that given a list concat l produces the same result as conc l.
     (20 points)

*)

(* -------------------------------------------------------------*)
(* QUESTION 2 :  Warm-up [10 points]                            *) 
(* -------------------------------------------------------------*)
(* remove_duplicates : ('a * 'a -> bool) -> 'a list -> 'a list *)

let rec filter p l = match l with
  | [] -> []
  | x :: l -> if p x then x :: filter p l else filter p l


let rec remove_duplicates eq l = match l with
| [] -> []
| h :: t -> h :: filter (fun x -> not(eq (x , h))) (remove_duplicates eq t)



(* -------------------------------------------------------------*)
(* QUESTION 3 :  Convolusion function  [15 points]               *) 
(* -------------------------------------------------------------*)

(* conv(f, g, dx) = f' 
   
  val conv: (real -> real) -> (real -> real) -> real -> (real -> real)
 *)


let iter_sum f (lo, hi) inc =
  let rec sum' lo r = 
      if (lo > hi) then r
    else 
      sum' (inc lo) (r +. f lo)
  in 
    sum' lo 0.0


let integral f (lo, hi) dx =
    dx *. iter_sum f ((lo +. (dx /. 2.0)), hi) (fun x -> (x +. dx))

let conv f g dy = (fun x -> integral (fun y -> ((f y) *. g(x +. y))) (0.0 , x) dy)



