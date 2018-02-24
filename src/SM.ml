open GT       
open List
open Syntax
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *) 

let rec eval (stack, state, input, output) prg = 
  match prg with
    | (cmd::progr) ->
      match cmd with
        | BINOP name -> 
          let (y, x) = (hd stack, hd (tl stack)) in  
          let expr = Expr.Binop (name, Expr.Const x, Expr.Const y) in 
          let value = Expr.eval state expr in
          eval (value :: stack, state, input, output) progr
        | CONST value ->  eval (value :: stack, state, input, output) progr
        | READ ->  eval (hd input :: stack, state, tl input, output) progr
        | WRITE ->   eval (tl stack, state, input, output @ [hd stack]) progr
        | LD name ->   eval (state name :: stack, state, input, output) progr
        | ST name ->   eval (tl stack, Expr.update name (hd stack) state, input, output) progr
    | _ -> (stack, state, input, output)

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

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