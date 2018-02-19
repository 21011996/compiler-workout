(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* An example of a non-trivial state: *)                                                   
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution. 
let _ =
  List.iter
    (fun x ->
       try  Printf.printf "%s=%d\n" x @@ s x
       with Failure s -> Printf.printf "%s\n" s
    ) ["x"; "a"; "y"; "z"; "t"; "b"]*)

(* Expression evaluator

     val eval : state -> expr -> int
 
   Takes a state and an expression, and returns the value of the expression in 
   the given state.
*)
let numOpToFunc name a b = 
  let func = match name with
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> (  / )
    | "%" -> ( mod )
    | _ -> failwith "Wrong numerical operator"
  in func a b

let cmpOpToFunc name a b = 
  let func = match name with
    | "<" -> ( < )
    | ">" -> ( > )
    | "<=" -> ( <= )
    | ">=" -> ( >= )
    | "==" -> ( = )
    | "!=" -> ( != )
    | _ -> failwith "Wrong compair operator"
  in if func a b then 1 else 0

let logicOpToFunc name a b = 
  let func = match name with
    | "&&" -> ( && )
    | "!!" -> ( || )
    | _ -> failwith "Wrong logic operator"
  in 
    let intToBool a = a != 0
    in if func (intToBool a) (intToBool b) then 1 else 0

let opSeparator name a b =
  let func = match name with
    | "+" | "-" | "*" | "/" | "%" -> numOpToFunc
    | "<" | ">" | "<=" | ">=" | "==" | "!=" -> cmpOpToFunc
    | "&&" | "!!" -> logicOpToFunc
    | _ -> failwith "Wrong operator"
  in func name a b

let eval state expr = 
  let rec recEval state expr = match expr with
    | Const value -> value
    | Var name -> state name
    | Binop (operator,left,right) -> opSeparator operator (recEval state left) (recEval state right)
  in recEval state expr                  
