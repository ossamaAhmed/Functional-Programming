  (* HOMEWORK 1 : COMP 302 Fall 2015
     
     PLEASE NOTE:  

     * All code files must be submitted electronically
       BEFORE class on 8 Oct 2015

    *  The submitted file name must be parser.ml 

    *  Submitted solutions will be graded according to their correctness and elegance. 
       Please consult the OCaml style guide posted on the course website.

    *  Your program must type-check and run usig OCaml of at least OCaml 4.0

    * Remove all "raise NotImplemented" with your solutions
  *)

  (* The code from the functional parsing lecture
     Author: Francisco Ferreira
   *)

  exception NotImplemented 

  (* Some string helper functions *)

  let to_list (s : string) : char list =
    let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

    let rec of_list (s : char list) : string =
    match s with
    | [] -> ""
    | c :: cs -> String.concat "" [String.make 1 c ; (of_list cs)]

    let is_digit (c : char) : bool = match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false

    let is_lower (c : char) : bool =
    (c >= 'a') && (c <= 'z')

    let is_upper (c : char) : bool =
    (c >= 'A') && (c <= 'Z')

    let is_letter (c : char) : bool =
    (is_upper c) || (is_lower c)

    let from_char (c:char) : string  = String.make 1 c

    (** FUNCTIONAL PARSERS **)

    (* The type of parsers *)

    type 'a parser = string -> ('a * string) option

    (* Two simple and useful parsers *)

    let return v : 'a parser =  fun inp -> Some (v,inp)

    let failure : 'a parser = fun inp -> None

    (* Still a simple parser *)

    let item : char parser =
    fun inp -> let inp' = to_list inp in
    match inp' with
    | [] -> None
    | (x::xs) -> Some (x, of_list xs)

    (* Using parsers *)

    let parse (p : 'a parser) (inp : string) = p inp

    (** Sequencing **)

    let (>>) p q =
    fun inp  -> match parse p inp with
    | None -> None
    | Some (_, out) -> parse q out

    let (>>=) p f =
    fun inp -> match parse p inp with
    | None -> None
    | Some (v, out) -> parse (f v) out

    (* example *)

    let p  = item >>= fun x ->
    item >>
    item >>= fun y ->
    return (x,y)


    (** Choice **)

    let (+++) p q =
    fun inp ->  match parse p inp with
    | None -> parse q inp
    | Some (v,out) -> Some (v, out)

  (*
  # parse (item +++ return 'd') "abc";;
  - : (char * string) option = Some ('a', "bc")

  # parse (failure +++ return 'd') "abc";;
  - : (char * string) option = Some ('d', "abc")
  *)

  (** derived primitives **)

  let sat : (char -> bool) -> char parser
  = fun p -> item >>= (fun v1 ->
  if p v1 then return v1 else failure)

  let digit : char parser
  = sat is_digit

  let lower : char parser
  = sat is_lower
  let upper : char parser
  = sat is_upper
  let letter : char parser
  = sat is_letter
  let alphanum : char parser
  = letter +++ digit

  let ch : char -> char parser
  = fun c -> sat (fun c' -> c' = c)

  (* Another interesting way of combining parsers is by repeatedly
  applying a parser, the following two parsers apply as many times as
  possible the parser passed as a parameter.

  many: applies zero or more times the parser while
  many1: requires the parser to at least succeed once.
  *)

  let rec many : 'a parser -> ('a list) parser
  = fun p -> (many1 p) +++ (return [])

  and many1 : 'a parser -> ('a list) parser
  = fun p -> p                >>= fun v1 ->
  many p            >>= fun v2 ->
  return (v1 :: v2)

  (* A parser that accepts natural numbers *)
  let nat = many1 digit >>= fun v1 ->
  return (int_of_string (of_list v1))

  (** Arithmetic expressions **)

  type expr =
  Num of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Exp of expr * expr
  | Sin of expr
  | Cos of expr
  | Neg of expr

  (* Q4.1: Factor the grammar considering the precedences:

  Expr E := E+E | E-E | E*E | E/E | sin E | cos E | E^E |-E | N
  Numbers N := nat

     Hint: Use the example in the lecture notes, you may need to add a new
     syntactic class. The precedence ordering you choose should be the same as in
     OCaml. 

   *)
  (*4.1 sol: our grammer follows the following precednece of the operators

      expression::= term + expression | term - expression | term
      term::= exponential * term | exponential / term | exponential
      exponential::= unary ^ exponential | unary
      unary::= - unary | sinusoid unary | cosine unary | fact
      fact::= nat
      nat::= 0|1|2|3....

    *)



  (* Q4.2: Implement the parser without parenthesis

     Note, we specify explicitly in the function definition the
     input and output types and annotated the input int with the type string and
     declared the return value of this fuction to be of type exp; this is not
     necessary, but clarifies our expectations that the function expr_no_parens
     takes a string as input and returns an expression.
   *)

  (*This parser checks if the input is a sinusoidal and returns the rest of the 
  input after removing sin from it*)

  let sinusoid = ch 's' >>= fun v1 -> 
  ch 'i' >>= fun v2 ->
  ch 'n' >>
  return ""

  (*This parser checks if the input is a cosine and returns the rest of the 
  input after removing cos from it*)

  let cosine = ch 'c' >>= fun v1 -> 
  ch 'o' >>= fun v2 ->
  ch 's' >>= fun v3 ->
  return ""

  let negation = ch '-' >>= fun v1 -> 
  return ""

  let rec expr_no_parens inp=
  ((term >>= fun v1 ->
   ch '+' >>
   expr_no_parens >>= fun v2 ->
   return (Plus (v1, v2)))
  +++
  (term >>= fun v1 ->
   ch '-' >>
   expr_no_parens >>= fun v2 ->
   return (Minus (v1, v2)))
  +++
  term) inp

  and term inp =

  ((exponential >>= fun v1 ->
   ch '*' >>
   term >>= fun v2 ->
   return (Mult (v1, v2)))
  +++
  (exponential >>= fun v1 ->
    ch '/' >>
    term >>= fun v2 ->
    return (Div (v1, v2)))
  +++
  exponential) inp

  and exponential inp =

  ((unary >>= fun v1 ->
    ch '^' >>
    exponential >>= fun v2 ->
    return (Exp (v1, v2)))
  +++
  unary) inp

  and unary inp =

  ((sinusoid 
    >> unary
    >>= fun v1->
    return (Sin v1))
  +++
  (cosine 
    >> unary
    >>= fun v1->
    return (Cos v1))
  +++
  (negation 
    >> unary
    >>= fun v1->
    return (Neg v1))
  +++
  fact) inp

  and fact inp = (nat >>= fun v1 -> return (Num v1)) inp

  (* Q4.3: Add parenthesis suppor to your factored grammar

  Expr E := E+E | E-E | E*E | E/E | sin E | cos E | E^E |-E | N | (E)
  Numbers N := nat

     Hint: don't factor the grammar again, use your answer for Q4.1 as a
     basis and extend it. 
   *)

  (*
    4.3 sol: Our old grammer is 
           expression::= term + expression | term - expression | term
           term::= exponential * term | exponential / term | exponential
           exponential::= unary ^ exponential | unary
           unary::= - unary | sinusoid unary | cosine unary | fact
           fact::= nat
           nat::= 0|1|2|3....

        we will add one precedence which is the paranthesis, so our grammer will be like 
           expression::= term + expression | term - expression | term
           term::= exponential * term | exponential / term | exponential
           exponential::= unary ^ exponential | unary
           unary::= - unary | sinusoid unary | cosine unary | paranthesis
           paranthesis::= (expression) | fact
           fact::= nat
           nat::= 0|1|2|3....
         *)


  (* Q4.4: Implement the complete parser by completing the function expr that
  takes a string as input and returns an expression.*)


  let rec expression inp =

   ((term >>= fun v1 ->
   ch '+' >>
   expression >>= fun v2 ->
   return (Plus (v1, v2)))
  +++
  (term >>= fun v1 ->
   ch '-' >>
   expression >>= fun v2 ->
   return (Minus (v1, v2)))
  +++
  term) inp

  and term inp =

  ((exponential >>= fun v1 ->
   ch '*' >>
   term >>= fun v2 ->
   return (Mult (v1, v2)))
  +++
  (exponential >>= fun v1 ->
    ch '/' >>
    term >>= fun v2 ->
    return (Div (v1, v2)))
  +++
  exponential) inp

  and exponential inp =

  ((unary >>= fun v1 ->
    ch '^' >>
    exponential >>= fun v2 ->
    return (Exp (v1, v2)))
  +++
  unary) inp

  and unary inp =

  ((sinusoid 
    >> unary
    >>= fun v1->
    return (Sin v1))
  +++
  (cosine 
    >> unary
    >>= fun v1->
    return (Cos v1))
  +++
  (negation 
    >> unary
    >>= fun v1->
    return (Neg v1))
  +++
  paranthesis) inp

  and paranthesis inp =
    ((ch '(' >>
      expression >>= fun v2 ->
      ch ')'>>
      return v2)
     +++
       fact) inp

  and fact inp = (nat >>= fun v1 -> return (Num v1)) inp

