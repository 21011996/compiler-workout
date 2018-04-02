(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}


    let empty_st x = failwith (Printf.sprintf "Undefined variable: %s" x)  
    (* Empty state *)
    let empty = {g = empty_st; l = empty_st; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = 
      let upt x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope
        then {s with l = upt x v s.l}
        else {s with g = upt x v s.g}
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if List.mem x s.scope then (s.l x) else (s.g x)

    (* Creates a new scope, based on a given state *)
    let enter st xs = {st with g = st.g; scope = xs; }

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
	      !(Ostap.Util.expr 
          (fun x -> x)
	        (Array.map (fun (a, s) -> a, 
                        List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
    )
          [|                
		        `Lefta, ["!!"];
		        `Lefta, ["&&"];
		        `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		        `Lefta, ["+" ; "-"];
		        `Lefta, ["*" ; "/"; "%"];
          |] 
	        )
	        primary);
    
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
    
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    let rec zip_with = function
      | x::xs, y::ys -> (x, y) :: (zip_with (xs, ys))
      | [], [] -> []
      | _, _ -> failwith "Uneven elem count in zip_with"

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> (State.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval env (eval env conf s1) s2
      | Skip -> conf
      | If (cond, stm1, stm2) -> 
        if Expr.eval st cond <> 0 
          then eval env conf stm1 
          else eval env conf stm2
      | While (cond, stm) -> 
        if Expr.eval st cond <> 0 
          then eval env (eval env conf stm) stmt 
          else conf
      | Repeat (stm, cond) ->  
        let (st_new, in_new, out_new) = eval env conf stm in
        let new_conf = (st_new, in_new, out_new) in
        if Expr.eval st_new cond <> 0 
          then new_conf 
          else eval env new_conf stmt
      | Call (name, args) ->
        let parms, vars, f_stm = env#definition name in
        let evaled_args = List.map (Expr.eval st) args in
        let assign st (var, valu) = State.update var valu st in
        let start_state = State.enter st (parms @ vars) in
        let local_state = List.fold_left assign start_state (zip_with (parms,evaled_args)) in
        let end_state, i_new, o_new = eval env (local_state, i, o) f_stm in
        (State.leave end_state st, i_new, o_new) 
                                
    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      stmt:
        %"read" "(" x:IDENT ")"          {Read x}
      | %"write" "(" e:expr ")" {Write e}
      | %"skip" {Skip}
      | %"if" e:expr
          %"then" the:parse
            elif:(%"elif" expr %"then" parse)*
          els:(%"else" parse)?
        %"fi" {
          If (e, the,
              List.fold_right
                (fun (e, t) elif -> If (e, t, elif)) 
                elif
                (match els with None -> Skip | Some s -> s)
          )
        }
      | %"while" e:expr %"do" s:parse %"od"  {While (e, s)}
      | %"for" i:parse "," c:expr "," s:parse %"do" b:parse %"od" {
          Seq (i, While (c, Seq (b, s)))
        }
      | %"repeat" s:parse %"until" e:expr  {Repeat (s, e)}
      | x:IDENT ":=" e:expr  {Assign (x, e)}
      | f:IDENT "(" args:mexprs ")" {Call (f, args)};
      expr:!(Expr.parse);
      mexprs: hd:expr tl:((-"," expr)*) {hd :: tl} | empty {[]}

    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: %"fun" name:IDENT "(" args:(names | none) ")" vars:(-(%"local") names | none)
        "{" body:!(Stmt.parse) "}" { name, (args, vars, body) } ;
      none: empty { [] } ;
      names: hd:IDENT tl:((-"," IDENT)* ) { hd :: tl }
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t

(* Top-level evaluator
     eval : t -> int list -> int list
   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String)
  in let defsMap = List.fold_left (fun map (f, data) -> M.add f data map) M.empty defs
  in let env = (object method definition f = M.find f defsMap end)
  in let _, _, output = Stmt.eval env (State.empty, i, []) body
  in output

(* Top-level parser *)
ostap (
  defs: !(Definition.parse) * ;

  parse: defs:defs body:!(Stmt.parse) { defs , body }
)