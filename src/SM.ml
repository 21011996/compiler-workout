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
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)  

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| JMP name :: _ -> eval env conf (env#labeled name)
| CJMP (cond, name) :: prg' -> 
  (match stack with
    | (x::new_stack) -> 
        (if (cond = "z" && x == 0) || (cond = "nz" && x != 0) 
          then eval env (cstack, new_stack, c) (env#labeled name)
          else eval env (cstack, new_stack, c) prg'
        )
    | _ -> failwith "SM:43 empty stack"
  )
| CALL (name, _, _) :: prg_next ->  eval env ((prg_next, st)::cstack, stack, c)(env#labeled name)
| END :: _ | RET _ :: _-> (match cstack with
              | (prg_prev, st_prev)::cstack_new ->
                  eval env (cstack_new, stack, (State.leave st st_prev, i, o)) prg_prev
              | [] -> conf)
| insn :: prg' ->
  let new_config = match insn with
      | BINOP op -> let y::x::stack' = stack in (cstack, Expr.to_func_renamed op x y :: stack', c)
      | READ     -> let z::i'        = i     in (cstack, z::stack, (st, i', o))
      | WRITE    -> let z::stack'    = stack in (cstack, stack', (st, i, o @ [z]))
      | CONST i  -> (cstack, i::stack, c)
      | LD x     -> (cstack, State.eval st x :: stack, c)
      | ST x     -> let z::stack' = stack in (cstack, stack', (State.update x z st, i, o))
      | LABEL name -> conf
      | BEGIN (_, p, l) -> let enter_st = State.enter st (p @ l) in
                           let upt_func = fun p (st, x::stack') -> (State.update p x st, stack') in
                           let (st', stack') = List.fold_right upt_func p (enter_st, stack) in
                           (cstack, stack', (st', i, o))
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

let compile (defs, p) =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, params) -> List.concat (List.map expr params) @ [CALL (f, List.length params, false)]
  in
  let rec compile_stm count = function  
  | Stmt.Read x ->  (count, [READ; ST x])
  | Stmt.Write e -> (count, expr e @ [WRITE])
  | Stmt.Assign (x, e) -> (count, expr e @ [ST x])
  | Stmt.Skip -> (count, [])
  | Stmt.Seq (st1, st2) -> 
      let (c1, prg1) = compile_stm count st1 in
      let (c2, prg2) = compile_stm c1 st2 in
      (c2, prg1 @ prg2)                                     
  | Stmt.If (cond, st1, st2) -> 
      let c1, prg1 = compile_stm count st1 in
      let label_then = make_label c1 in
      let c2, prg2 = compile_stm (c1+1) st2 in
      let label_else = make_label c2 in
      (c2+1, expr cond @ [CJMP ("z", label_then)] @ prg1 @ [JMP label_else; LABEL label_then] @ prg2 @ [LABEL label_else])                        
  | Stmt.While (cond, st) -> 
      let label_loop = make_label count in
      let (c1, prg1) = compile_stm (count+1) st in
      let label_check = make_label c1 in
      (c1+1, [JMP label_check; LABEL label_loop] @ prg1 @ [LABEL label_check] @ expr cond @ [CJMP ("nz", label_loop)])
  | Stmt.Repeat (st, cond) ->  
      let label_loop = make_label count in
      let (c1, prg1) = compile_stm (count+1) st in
      (c1, [LABEL label_loop] @ prg1 @ expr cond @ [CJMP ("z", label_loop)])
  | Stmt.Call (name, args) -> 
      let args_prg = List.concat @@ List.map expr args in
      (count, args_prg @ [CALL (name, List.length args, true)])
  | Stmt.Return stm -> (count, (match stm with Some exp -> expr exp @ [RET true] | None -> [RET false]))
  in 
  let compile_def count_r (name, (params, locals, body)) =
    let (c1, func_prg) = compile_stm count_r body in
    (c1, [LABEL name; BEGIN (name, params, locals)] @ func_prg @ [END])
  in
  let start_count = 0
  in
  let count_start, defs_prg = List.fold_left
      (fun (counter, prgs)(name, configur) -> 
          let (count_z, prg_new) = compile_def counter (name, configur) 
          in (count_z, prg_new::prgs))
      (start_count, [])
      defs
  in
  let (_, prg) = compile_stm count_start p in
  let label_main = "L_main"
  in prg @ [END] @ List.concat defs_prg