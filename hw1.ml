(*
 * Lucien GEORGE: 260571775
 * Ossama AHMED: 260549558
 *)

(* HOMEWORK 1 : COMP 302 Fall 2015
   
   PLEASE NOTE:  

   * All code files must be submitted electronically
     BEFORE class on 24 Sep, 2015

  *  The submitted file name must be hw1.ml 

  *  Submitted solutions will be graded according to their correctness and elegance. 
     Please consult the OCaml style guide posted on the course website.

  *  Your program must type-check and run usig OCaml of at least OCaml 4.0

  * Remove all "raise NotImplemented" with your solutions
*)

exception NotImplemented
exception Error


(* ------------------------------------------------------------*)
(* QUESTION : Zipping and Unzipping                            *)
(* ------------------------------------------------------------*)

let rec zip l1 l2 = match l1 , l2 with
| [] , [] -> []
| [] , l2 -> []
| l1 , [] -> []
| h1 :: t1 , h2 :: t2 -> (h1 , h2) :: zip t1 t2
 

let rec unzip l = match l with
| [] -> ([] , [])
| (h1 , h2) :: t -> let (t1 , t2) = unzip t in (h1::t1 , h2::t2)


(*

PROVE THE FOLLOWING STATEMENT:

Theorem: unzip (zip l1 l2) = (l1, l2)

Base Case:
_ l1 is empty: zip [] l2 => [] (By program zip)
               unzip []  => ([] , []) (By program unzip)

_ l2 is empty: zip l2 [] => [] (By program zip)
               unzip [] => ([] , []) (By program unzip)

Step Case: l1 = h1 :: t1
           l2 = h2 :: t2
_Induction Hypothesis: unzip (zip t1 t2) = (t1 , t2)
unzip (zip l1 l2) => unzip ((h1 , h2) :: (zip t1 t2)) (By program zip)
                  => unzip (h1 , h2) :: unzip (zip t1 t2) (By distribution)
                  => unzip ((h1 , h2) :: (t1 , t2)) (By induction hypothesis)
                  => (h1 :: t1 , h2 :: t2) (By program unzip)
                  => (l1 , l2)
Done.

*)



(* ------------------------------------------------------------*)
(* QUESTION : Pocket Calculator                                *)
(* ------------------------------------------------------------*)

type instruction = Plus | Minus | Times | Div | Sin | Cos | Exp | Float of float

type stack = float list

let rec instr i s = match i , s with
| Float e1 , (s:stack) -> ([e1] @ s)
| Plus , e1 :: e2 :: t -> instr (Float (e1 +. e2)) t
| Minus , e1 :: e2 :: t -> instr (Float (e1 -. e2)) t
| Times , e1 :: e2 :: t -> instr (Float (e1 *. e2)) t
| Div , e1 :: e2 :: t -> instr (Float (e1 /. e2)) t
| Sin , e1 ::t -> instr (Float (sin e1)) t
| Cos , e1 ::t -> instr (Float (cos e1)) t
| Exp , e1 :: e2 :: t -> instr (Float (e1 ** e2)) t
| _ , _ -> raise Error (* Error Handling *)


(* the two functions progHelper and to_float are helper functions for question 3.4*)

let rec progHelper instrs = match instrs with
| [] -> [Float 0.0]
| (Float h1) :: (Float h2) :: Plus :: t -> progHelper((Float (h1 +. h2)) :: t)
| (Float h1) :: (Float h2) :: Minus :: t -> progHelper((Float (h1 -. h2)) :: t)
| (Float h1) :: (Float h2) :: Times :: t -> progHelper((Float (h1 *. h2)) :: t)
| (Float h1) :: (Float h2) :: Div :: t -> progHelper((Float (h1 /. h2)) :: t)
| (Float h1) :: Sin :: t -> progHelper((Float (sin h1)) :: t)
| (Float h1) :: Cos :: t -> progHelper((Float (sin h1)) :: t)
| (Float h1) :: (Float h2) :: Exp :: t -> progHelper((Float (h1 ** h2)) :: t)
| [(Float h1)] -> [(Float h1)]
| _ -> raise Error (* Error Handling *)

let to_float l = match l with
| [] -> 0.0
| [Float h] -> h

let rec prog instrs = to_float (progHelper instrs)


type exp = 
  | PLUS  of exp * exp  (* Plus *)
  | MINUS of exp * exp  (* Minus *)
  | TIMES of exp * exp  (* Times *)
  | DIV   of exp * exp  (* Div *)
  | SIN   of exp        (* Sin *)
  | COS   of exp        (* Cos *)
  | EXP   of exp * exp  (* Exp *)
  | FLOAT of float


let rec eval e = match e with
| FLOAT e1 -> e1
| PLUS (e1 , e2) -> (eval e1) +. (eval e2)
| MINUS (e1 , e2) -> (eval e1) -. (eval e2)
| TIMES (e1 , e2) -> (eval e1) *. (eval e2)
| DIV (e1 , e2) -> (eval e1) /. (eval e2)
| SIN e1 -> sin (eval e1)
| COS e1 -> cos (eval e1)
| EXP (e1 , e2) -> (eval e1) ** (eval e2)


let rec to_instr e = match e with
| FLOAT e1 -> [Float e1]
| PLUS (e1 , e2) -> to_instr e1 @ to_instr e2 @ [Plus]
| MINUS (e1 , e2) -> to_instr e1 @ to_instr e2 @ [Minus]
| TIMES (e1 , e2) -> to_instr e1 @ to_instr e2 @ [Times]
| DIV (e1 , e2) -> to_instr e1 @ to_instr e2 @ [Div]
| SIN e1 -> to_instr e1 @ [Sin]
| COS e1 -> to_instr e1 @ [Cos]
| EXP (e1 , e2) -> to_instr e1 @ to_instr e2 @ [Exp]












