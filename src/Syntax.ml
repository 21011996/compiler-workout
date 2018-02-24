(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
open List
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

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

    (* Expression evaluator

          val eval : state -> t -> int
 
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

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)

    let rec eval (state, input, output) statement = match statement with
        | Read name -> (Expr.update name (hd input) state, tl input, output)
        | Write expr -> (state, input, output @ [Expr.eval state expr])
        | Assign (name, expr) ->  (Expr.update name (Expr.eval state expr) state, input, output)
        | Seq (statement1, statement2) -> eval (eval (state, input, output) statement1) statement2
                                                         
  end
