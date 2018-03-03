open GT       
open List
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list


(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *) 

let rec eval config prg = 
  let rec eval_inner (stack, (state, input, output)) cmd = 
    match cmd with
      | BINOP name -> 
        let (y, x) = (hd stack, hd (tl stack)) in  
        let expr = Expr.Binop (name, Expr.Const x, Expr.Const y) in 
        let value = Expr.eval state expr in
        (value :: stack, (state, input, output))
      | CONST value ->  (value :: stack, (state, input, output))
      | READ ->  (hd input :: stack, (state, tl input, output))
      | WRITE ->   (tl stack, (state, input, output @ [hd stack]))
      | LD name ->   (state name :: stack, (state, input, output))
      | ST name ->  (tl stack, (Expr.update name (hd stack) state, input, output))
  in fold_left eval_inner config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpr expr = match expr with
    | Expr.Var name ->  [LD name]
    | Expr.Const value -> [CONST value]
    | Expr.Binop (op, a, b) -> compileExpr a @ compileExpr b @ [BINOP op]

let rec compile statement = match statement with 
    | Stmt.Assign (name, expr) -> compileExpr expr @ [ST name]
    | Stmt.Read name -> [READ] @ [ST name]
    | Stmt.Write expr ->  compileExpr expr @ [WRITE]
    | Stmt.Seq (stmt1, stmt2) ->  compile stmt1 @ compile stmt2
