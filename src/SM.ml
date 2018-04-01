open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env ((stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| JMP name :: _ -> eval env conf (env#labeled name)
| CJMP (cond, name) :: prg' -> 
  (match stack with
    | (x::new_stack) -> 
        (if (cond = "z" && x = 0) || (cond = "nz" && x <> 0) 
          then eval env (new_stack, c) (env#labeled name)
          else eval env (new_stack, c) prg'
        )
    | _ -> failwith "SM:43 empty stack"
  )
| insn :: prg' ->
  let new_config = match insn with
      | BINOP op -> let y::x::stack' = stack in (Expr.to_func op x y :: stack', c)
      | READ     -> let z::i'        = i     in (z::stack, (st, i', o))
      | WRITE    -> let z::stack'    = stack in (stack', (st, i, o @ [z]))
      | CONST i  -> (i::stack, c)
      | LD x     -> (st x :: stack, c)
      | ST x     -> let z::stack'    = stack in (stack', (Expr.update x z st, i, o))
      | LABEL name -> conf
      | _ -> failwith "SM:54 weird"
  in eval env new_config prg'

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let make_label n = "L_" ^ (string_of_int n)

let compile p =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  let rec compile' count = function  
  | Stmt.Read x ->  (count, [READ; ST x])
  | Stmt.Write e -> (count, expr e @ [WRITE])
  | Stmt.Assign (x, e) -> (count, expr e @ [ST x])
  | Stmt.Skip -> (count, [])
  | Stmt.Seq (st1, st2) -> 
    let (c1, prg1) = compile' count st1 in
    let (c2, prg2) = compile' c1 st2 in
    (c2, prg1 @ prg2)                                     
  | Stmt.If (cond, st1, st2) -> 
    let c1, prg1 = compile' count st1 in
    let label_then = make_label c1 in
    let c2, prg2 = compile' (c1+1) st2 in
    let label_else = make_label c2 in
    (c2+1, expr cond @ [CJMP ("z", label_then)] @ prg1 @ [JMP label_else; LABEL label_then] @ prg2 @ [LABEL label_else])                        
  | Stmt.While (cond, st) -> 
    let label_loop = make_label count in
    let (c1, prg1) = compile' (count+1) st in
    let label_check = make_label c1 in
    (c1+1, [JMP label_check; LABEL label_loop] @ prg1 @ [LABEL label_check] @ expr cond @ [CJMP ("nz", label_loop)])
  | Stmt.Repeat (st, cond) ->  
    let label_loop = make_label count in
    let (c1, prg1) = compile' (count+1) st in
    (c1, [LABEL label_loop] @ prg1 @ expr cond @ [CJMP ("z", label_loop)])
in let (t_count, prg) = compile' 0 p
in prg
