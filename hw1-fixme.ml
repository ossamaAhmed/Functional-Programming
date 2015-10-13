(*
   Purpose: inc the number n by k
            assumes n >= 0

   Examples:
   inc 0  4 ==> 4
   inc 3  4 ==> 7

 
*)

  let rec inc n (k:int) = match n with
  | 0 -> (k:int)
  | _ ->  1 + inc (n-1) k


(*
   Purpose: compute n! = n * (n-1) * (n-2) * ... * 1
            assumes n >= 0

   Examples:
   fact 0 ==> 1
   fact 5 ==> 120

   fact : int -> int
*)

let rec fact (n : int)  = match n with 
 |   0 -> 1
 |  _ -> n * (fact (n - 1))


