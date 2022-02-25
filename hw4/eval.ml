open Ast

(** Variable set. Based on OCaml standard library Set. *)
module VarSet = Set.Make (String)

(* Helper function for parsing an expression. Useful for testing. *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
(*******************************************************************|
|**********************   Interpreter   ****************************|
|*******************************************************************|
|*******************************************************************)

(* Exception indicating that evaluation is stuck *)
exception Stuck of string

(* Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* Raises an exception for things that need to be implemented
 * in this assignment *)
let todo () = failwith "TODO"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : VarSet.t =
  let s=VarSet.empty in 
  match e with
    | Var x -> VarSet.add x s
    | Lambda (x,y) -> (let s1=free_vars y in
                      let s2=VarSet.remove x s1 in
                        VarSet.union s s2)
    | App (e1,e2) -> (let s1=VarSet.union (free_vars e1) (free_vars e2) in
                      VarSet.union s s1)
    | LetBind (x,e1,e2) -> (let s1=free_vars e1 in
                            let s2=free_vars e2 in
                            let s3=VarSet.remove x s2 in
                            let s4=VarSet.union s3 s1 in
                            VarSet.union s s4)
    | NumLit _ | ListNil -> s
    | IfThenElse (e1,e2,e3) -> (let s1=free_vars e1 in
                                let s2=free_vars e2 in
                                let s3=free_vars e3 in
                                let s4= VarSet.union s1 s2 in
                                let s5= VarSet.union s4 s3 in
                                VarSet.union s5 s)
    | Fix e1 -> VarSet.union s (free_vars e1)
    | Binop (e1, op, e2) -> (let s1= free_vars e1 in
                              let s2= free_vars e2 in
                              let s3= VarSet.union s1 s2 in
                              VarSet.union s3 s)
    | ListCons (e1,e2) -> (let s1= free_vars e1 in
                              let s2= free_vars e2 in
                              let s3= VarSet.union s1 s2 in
                              VarSet.union s3 s)
    | ListHead e1 -> VarSet.union s (free_vars e1)
    | ListTail e1 -> VarSet.union s (free_vars e1)
    | ListIsNil e1 -> VarSet.union s (free_vars e1)
    | _ -> im_stuck "wrong free_vars implementation"
  ;;

let rec findNext (x: string) (s: VarSet.t) (n: int) : string =
  let new_x=x^(string_of_int n) in
  if VarSet.mem new_x s then findNext x s (n+1)
  else new_x
  ;;

(** Performs the substitution [x -> e1]e2 *)
let rec subst (x : string) (e1 : expr) (e2 : expr) : expr = 
  match e2 with
    | Var y -> (if x=y then e1 else e2)
    | NumLit n -> NumLit (n)
    | ListNil -> ListNil
    | App (f,g) -> App (subst x e1 f ,subst x e1 g) 
    | Lambda (y,e) -> if x=y then Lambda (x,e)
                      else 
                        let free_e1= free_vars (e1) in
                        if VarSet.mem y free_e1 then 
                          let free_e= free_vars (e) in 
                          let free_union= VarSet.union free_e1 free_e in
                          let z= findNext y free_union 0 in
                          let expz= Var z in 
                          Lambda (z, subst x e1 (subst y expz e))
                        else Lambda (y, subst x e1 e)
    | Binop (f,op, g) -> Binop(subst x e1 f, op, subst x e1 g)
    | LetBind (s, f, g) -> if x=s then LetBind (s, subst x e1 f, g)
                            else LetBind (s, subst x e1 f, subst x e1 g)
    | IfThenElse (a,b,c) -> IfThenElse (subst x e1 a, subst x e1 b, subst x e1 c)
    | ListCons (a,b) -> ListCons (subst x e1 a, subst x e1 b)
    | ListHead e -> ListHead (subst x e1 e)
    | ListTail e -> ListTail (subst x e1 e)
    | ListIsNil e -> ListIsNil (subst x e1 e)
    | Fix e -> Fix (subst x e1 e)
    | _ -> im_stuck "wrong subst implementation"
  ;;


(** Evaluates e. You need to copy over your
   implementation of homework 3. *)
let rec eval (e : expr) : expr =
  try
    match e with
    (* Things you need to implement *)
    | NumLit n -> NumLit (n)
    | Binop (e1, op, e2) -> (let s1= eval e1 in
                            let s2= eval e2 in
                            match s1,s2 with
                            | NumLit t1, NumLit t2 -> (match op with
                                                      | Add -> NumLit(t1+t2)
                                                      | Sub -> NumLit(t1-t2)
                                                      | Mul -> NumLit(t1*t2)
                                                      | Gt -> if t1>t2 then NumLit(1) else NumLit(0)
                                                      | Lt -> if t1<t2 then NumLit(1) else NumLit(0)
                                                      | And -> if (t1<>0 && t2<>0) then NumLit(1) else NumLit(0)
                                                      | Or -> if (t1<>0 || t2<>0) then NumLit(1) else NumLit(0)
                                                      | Eq -> if t1=t2 then NumLit(1) else NumLit(0))
                                                      
                            | _ -> im_stuck "aaa")
    | IfThenElse (e1, e2, e3) -> (let s1= eval e1 in
                                  match s1 with
                                  | NumLit first when (first<>0) -> eval e2
                                  | NumLit second when (second=0) -> eval e3
                                  | _ -> im_stuck "bbb")
    | ListNil -> ListNil
    | ListCons (e1, e2) -> (let s1= eval e1 in
                            let s2= eval e2 in 
                            match s1,s2 with
                            | s1,s2 when ((is_value s1) && (is_value s2)) -> ListCons(s1,s2)
                            | _ -> im_stuck "ccc")
    | ListHead e -> ( let s= eval e in
                      match s with 
                      | ListCons(v1, v2) when ((is_value (eval v1)) && (is_value (eval v2))) -> (eval v1)
                      | _ -> im_stuck "eee")
    | ListTail e -> ( let s= eval e in
                      match s with
                      | ListCons(v1, v2) when ((is_value (eval v1)) && (is_value (eval v2)))-> (eval v2)
                      | _ -> im_stuck "fff")
    | ListIsNil e -> (let s= eval e in
                      match s with
                      | ListNil -> NumLit (1)
                      | ListCons (v1,v2) when ((is_value (eval v1)) && (is_value (eval v2))) -> NumLit (0)
                      | _ -> im_stuck "ddd")
    (* Things you don't need to implement in this assignment *)
    | Var _ -> im_stuck "variables are stuck for default"
    | LetBind (s,e1,e2) -> (let v1= eval e1 in 
                            if is_value (v1) then 
                              let result=eval (subst s v1 e2) in
                              if is_value (result) then result
                              else im_stuck "result letbind wrong implementation"
                            else im_stuck "let bind wrong implementation")
    | Lambda (s,e) -> Lambda (s,e)
    | App (e1,e2) -> (match eval e1 with
                      | Lambda (s,epslon) -> (let v= eval e2 in
                                              if is_value (v) then
                                                let result= eval (subst s v epslon) in
                                                if is_value (result) then result 
                                                else im_stuck "result wrong implementation"
                                              else im_stuck "e2 is not value")
                      | _ -> im_stuck "e1 is not lambda abstraction")
    | Fix e -> (let v= eval e in
                match v with
                | Lambda (f, epslon) -> ( let fix_rec= Fix v in 
                                          let v1= eval (subst f fix_rec epslon) in
                                          if is_value (v1) then v1
                                          else im_stuck "fix v1 implementation wrong")
                | _ -> im_stuck "fix not match")
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)
  ;;