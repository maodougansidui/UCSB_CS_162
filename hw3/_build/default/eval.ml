
open Ast

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

(* Raises an exception for things to be in the next assignment *)
let hw4 () = failwith "Homework 4"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")

(* Evaluates expression e *)
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
    | _ -> hw4 ()
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)