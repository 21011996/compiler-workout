open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string
(* create an S-expression          *) | SEXP    of string * int
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool
(* drops the top element off       *) | DROP
(* duplicates the top element      *) | DUP
(* swaps two top elements          *) | SWAP
(* checks the tag of S-expression  *) | TAG     of string
(* enters a scope                  *) | ENTER   of string list
(* leaves a scope                  *) | LEAVE
with show
                                                   
(* The type for the stack machine program *)
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)  
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| JMP name :: _ -> eval env conf (env#labeled name)
| CJMP (cond, name) :: prg' -> 
  (match stack with
    | (x::new_stack) -> 
        (if (cond = "z" && Value.to_int x == 0) || (cond = "nz" && Value.to_int x != 0) 
          then eval env (cstack, new_stack, c) (env#labeled name)
          else eval env (cstack, new_stack, c) prg'
        )
    | _ -> failwith "SM:43 empty stack"
  )
| CALL (name, n, p) :: prg_next ->  
  if env#is_label name
    then eval env ((prg_next, st)::cstack, stack, c)(env#labeled name)
    else eval env (env#builtin conf name n p) prg_next
| END :: _ | RET _ :: _-> (match cstack with
              | (prg_prev, st_prev)::cstack_new ->
                  eval env (cstack_new, stack, (State.leave st st_prev, i, o)) prg_prev
              | [] -> conf)
| insn :: prg' ->
  let new_config = match insn with
      | BINOP op -> let y::x::stack' = stack in (cstack, Value.of_int (Expr.to_func_renamed op (Value.to_int x) (Value.to_int y)) :: stack', c)
      (*| READ     -> let z::i'        = i     in (cstack, z::stack, (st, i', o))
      | WRITE    -> let z::stack'    = stack in (cstack, stack', (st, i, o @ [z]))*)
      | CONST i  -> (cstack, (Value.of_int i)::stack, c)
      | LD x     -> (cstack, State.eval st x :: stack, c)
      | ST x     -> let z::stack' = stack in (cstack, stack', (State.update x z st, i, o))
      | LABEL name -> conf
      | BEGIN (_, p, l) -> let enter_st = State.enter st (p @ l) in
                           let upt_func = fun p (st, x::stack') -> (State.update p x st, stack') in
                           let (st', stack') = List.fold_right upt_func p (enter_st, stack) in
                           (cstack, stack', (st', i, o))
      | STRING s -> (cstack, (Value.of_string s)::stack, c)
      | STA (x, n) -> 
        let v::is, stack' = split (n+1) stack in
        (cstack, stack', (Stmt.update st x v @@ List.rev is, i, o))
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
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (defs, p) = 
  let label s = "L" ^ s in
  let rec call f args p =
    let args_code = List.concat @@ List.map expr args in
    args_code @ [CALL (label f, List.length args, p)]
  and pattern lfalse _ = failwith "Not implemented"
  and bindings p = failwith "Not implemented"
  and expr e = failwith "Not implemented" in
  let rec compile_stmt l env stmt =  failwith "Not implemented" in
   let compile_def env (name, (args, locals, stmt)) =
     let lend, env       = env#get_label in
     let env, flag, code = compile_stmt lend env stmt in
     env,
     [LABEL name; BEGIN (name, args, locals)] @
     code @
     (if flag then [LABEL lend] else []) @
     [END]
   in
   let env =
     object
       val ls = 0
       method get_label = (label @@ string_of_int ls), {< ls = ls + 1 >}
     end
   in
   let env, def_code =
     List.fold_left
       (fun (env, code) (name, others) -> let env, code' = compile_def env (label name, others) in env, code'::code)
       (env, [])
       defs
   in
   let lend, env = env#get_label in
   let _, flag, code = compile_stmt lend env p in
   (if flag then code @ [LABEL lend] else code) @ [END] @ (List.concat def_code) 