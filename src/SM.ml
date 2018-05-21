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
      | BINOP op -> let y::x::stack' = stack in (cstack, Value.of_int (Expr.to_func op (Value.to_int x) (Value.to_int y)) :: stack', c)
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
      | SEXP (t, n) ->  
        let elems, stack_new = split (n+1) stack in
          (cstack, Value.sexp t (List.rev elems):: stack_new, c)
      | STA (x, n) -> 
        let v::is, stack' = split (n+1) stack in
        (cstack, stack', (Stmt.update st x v @@ List.rev is, i, o))
      | DROP -> 
        let _::stack_new = stack in
        (cstack, stack_new, c)
      | DUP ->  
        let x::_ = stack in
        (cstack, x::stack, c)
      | SWAP -> 
        let a::b::stack_new = stack in
        (cstack, b::a::stack_new, c)
      | TAG s ->  
        let a::stack_new = stack in
        let res = match a with
          | Value.Sexp (t, _) ->  Value.of_int @@ if t = s then 1 else 0
          | _ ->  Value.of_int 0
        in
        (cstack, res::stack_new, c)
      | ENTER xs ->  
        (cstack, stack, (State.push st State.undefined xs, i, o))
      | LEAVE ->  
        (cstack, stack, (State.drop st, i, o))
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
class env =
	object (self)
		val mutable label = 0
		method next_label = let last_label = label in
			label <- label + 1; Printf.sprintf "L%d" last_label
	end

let rec list_init i n f = if i >= n then [] else (f i) :: (list_init (i + 1) n f)

let rec compile' to_drop lend env p =
  let label s = "L" ^ s in
  let rec max_of = function
    | []      -> 0
    | x::rest -> max x @@ max_of rest
  in
  let rec depth = function
    | Stmt.Pattern.Wildcard -> 1
    | Stmt.Pattern.Ident _  -> 1
    | Stmt.Pattern.Sexp (_, elems) -> 1 + (max_of @@ List.map depth elems)
  in
  let rec call f args p =
    let args_code = List.concat @@ List.map expr args in
    args_code @ [CALL (label f, List.length args, p)]
  and pattern on_stack drop_labels = function
    | Stmt.Pattern.Wildcard -> [DROP]
    | Stmt.Pattern.Ident _ ->  [DROP]
    | Stmt.Pattern.Sexp (tag, elems) -> [DUP; TAG tag; CJMP ("z", List.nth drop_labels on_stack)] @
    (List.concat @@ List.mapi (fun i epatt -> [DUP; CONST i; CALL (label ".elem", 2, false)] @ pattern (on_stack + 1) drop_labels epatt) elems) @ [DROP]
  and bindings' vars = function
    | Stmt.Pattern.Wildcard -> vars, [DROP]
    | Stmt.Pattern.Ident x -> x::vars, [ST x]
    | Stmt.Pattern.Sexp (_, elems) ->
        let new_vars, p = (List.fold_left
            (fun (v, p) (i, patt) -> let new_v, pr = bindings' v patt in new_v, (p @ [DUP; CONST i; CALL (label ".elem", 2, false)] @ pr))
            (vars, [])
            (List.mapi (fun i e -> (i, e)) elems)
        ) in
        new_vars, p @ [DROP]
  and bindings p =
    let vars, prog = bindings' [] p in
    (ENTER vars) :: prog
  and expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, params) -> call f params false
  | Expr.String s         -> [STRING s]
  | Expr.Array elems      -> call ".array" elems false
  | Expr.Elem (a, i)      -> call ".elem" [a; i] false
  | Expr.Length a         -> call ".length" [a] false
  | Expr.Sexp (tag, es)   -> (List.concat @@ List.map expr es) @ [SEXP (tag, List.length es)]
  and case_branch (patt, body) case_end =
    let d = depth patt in
    let drop_labels = list_init 0 (d + 1) (fun _ -> env#next_label) in
    let drop_list = List.rev @@ List.tl @@ List.concat @@ List.map (fun s -> [DROP; LABEL s]) drop_labels in
    let _, cbody = compile' (to_drop + 1) lend env body in
    [DUP] @ pattern 1 drop_labels patt @ [DUP] @ bindings patt @ cbody @ [LEAVE; JMP case_end] @ drop_list
  in
  match p with
  | Stmt.Seq (s1, s2)      -> let f1, p1 = compile' to_drop lend env s1 in
                              let f2, p2 = compile' to_drop lend env s2 in
                              f1 || f2, p1 @ p2
  | Stmt.Assign (x, [], e) -> false, expr e @ [ST x]
  | Stmt.Assign (x, is, e) -> false, List.concat (List.map expr is) @ expr e @ [STA (x, List.length is)]
  | Stmt.Skip              -> false, []
  | Stmt.If (e, s1, s2)    -> let fLabel = env#next_label in
  						      let eLabel = env#next_label in
  						      let f1, p1 = compile' to_drop lend env s1 in
  						      let f2, p2 = compile' to_drop lend env s2 in
  						      f1 || f2, expr e @ [CJMP ("z", fLabel)] @
  						      p1 @ [JMP eLabel; LABEL fLabel] @
  						      p2 @ [LABEL eLabel]
  | Stmt.While (e, s)      -> let loopLabel = env#next_label in
  						      let endLabel  = env#next_label in
  						      let f, p = compile' to_drop lend env s in
  						      f, [LABEL loopLabel] @ expr e @ [CJMP ("z", endLabel)] @
  						      p @ [JMP loopLabel; LABEL endLabel]
  | Stmt.Repeat (s, e)     -> let startLabel = env#next_label in
                              let f, p = compile' to_drop lend env s in
  						      f, [LABEL startLabel] @ p @ expr e @ [CJMP ("z", startLabel)]
  | Stmt.Call (f, p)       -> false, call f p true
  | Stmt.Return r          -> false, (list_init 0 to_drop (fun _ -> DROP)) @ (match r with | None -> [RET false] | Some v -> expr v @ [RET true])
  | Stmt.Case (e, br)      -> let case_end = env#next_label in
                              false, expr e @ (List.concat @@ List.map (fun b -> case_branch b case_end) br) @ [LABEL case_end; DROP]

(* Stack machine compiler
     val compile : Language.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (defs, p) =
  let label s = "L" ^ s in
  let compile_def env (name, (args, locals, stmt)) =
    let lend       = env#next_label in
    let flag, code = compile' 0 lend env stmt in
    env,
    [LABEL name; BEGIN (name, args, locals)] @
    code @
    (if flag then [LABEL lend] else []) @
    [END]
  in
  let env = new env in
  let env, def_code =
    List.fold_left
      (fun (env, code) (name, others) -> let env, code' = compile_def env (label name, others) in env, code'::code)
      (env, [])
      defs
  in
  let lend = env#next_label in
  let flag, code = compile' 0 lend env p in
  (if flag then code @ [LABEL lend] else code) @ [END] @ (List.concat def_code)