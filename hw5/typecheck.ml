open Ast

module Env = Map.Make (String) type env = typ Env.t

exception Type_error of string 
let ty_err msg = raise (Type_error msg)

let todo () = failwith "TODO"

let rec typecheck (env : env) (e : expr) : typ =
  try
  match e with
  | NumLit _ -> TInt
  | Var s -> (if Env.mem s env then Env.find s env else ty_err "variables not valid type")
  | Binop (e1, op, e2) -> (let s1= typecheck env e1 in
                           let s2= typecheck env e2 in
                           match s1, s2 with
                            | TInt , TInt -> (match op with
                                                | Add -> TInt
                                                | Sub -> TInt
                                                | Mul -> TInt
                                                | And -> TInt
                                                | Or -> TInt
                                                | Gt -> TInt
                                                | Lt -> TInt
                                                | Eq -> TInt)
                            | TFun _, TFun _ -> (match op with
                                                | Eq -> TInt
                                                | _ -> ty_err "binop tfun tfun nonapplicable")
                            | TList _, TList _ -> (match op with
                                                | Eq -> TInt
                                                | _ -> ty_err "binop tlist tlist nonapplicable")
                            | _ -> ty_err "binop don care failure")
  | IfThenElse (e1, e2, e3) -> (let s1= typecheck env e1 in
                                let s2 = typecheck env e2 in
                                let s3 = typecheck env e3 in
                                match s1 with
                                | TInt when (s2=s3) -> s2
                                | _ -> ty_err "if then else don care failure")
  | ListNil (x: typ option) -> (match x with 
                                | Some t -> TList (t)
                                | None -> ty_err "for ListNil no type specified")
  | ListCons (e1, e2) -> (let t1= typecheck env e1 in
                          let t2= typecheck env e2 in
                          match t1, t2 with
                          | t1, TList (s1) when (t1=s1) -> TList(s1) 
                          | _ -> ty_err "listCons don care failure")
  | ListHead ed -> (match typecheck env ed with
                    | TList (t) -> t
                    | _ -> ty_err "listHead don care failure")
  | ListTail ed -> (match typecheck env ed with
                    | TList (t) -> TList (t)
                    | _ -> ty_err "listTail don care failure")
  | ListIsNil ed -> (match typecheck env ed with
                    | TList _ -> TInt
                    | _ -> ty_err "listIsNil don care failure")
  | LetBind (s, (t:typ option), e1, e2) -> (match t with
                                            | Some t1 -> (let t3= typecheck env e1 in
                                                          if t1=t3 then 
                                                            let newEnv=Env.add s t1 env in
                                                            typecheck newEnv e2 
                                                          else ty_err "pp_nested letbind failure")
                                            | None -> ty_err "letbind no type specified") 
  | Lambda (s, (t:typ option),ed) -> (match t with
                                    | Some t1 -> (let newEnv= Env.add s t1 env in
                                                  let t2=typecheck newEnv ed in
                                                  TFun (t1,t2))
                                    | None -> ty_err "lambda no type specified")
  | App (e1, e2) -> (let s1= typecheck env e1 in
                     let s2= typecheck env e2 in
                     match s1,s2 with
                     | TFun (t1,t2), t3 when (t1=t3) -> t2
                     | _ -> ty_err "app don care failure")
  | Fix ed -> (match typecheck env ed with
                | TFun (t1,t2) when (t1=t2) -> t1
                | _ -> ty_err "fix don care failure") 
  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)