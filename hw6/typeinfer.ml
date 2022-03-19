open Ast

exception Type_error = Typecheck.Type_error ;;
let ty_err = Typecheck.ty_err ;;

(** Type AST used for type inference *)
type ityp =
  | (* Type variable *)
    IInt
  | (* Function type *)
    IFun of ityp * ityp
  | (* List type *)
    IList of ityp
  | (* Integer type *)
    IVar of int

module Dsl = struct
  let i = IInt
  let (=>) t1 t2 = IFun (t1, t2)
  let l t = IList t
  let v n = IVar n
end

(** Converts a typ into an ityp *)
let rec ityp_of_typ = function
  | TInt -> IInt
  | TFun (t1, t2) -> IFun (ityp_of_typ t1, ityp_of_typ t2)
  | TList t -> IList (ityp_of_typ t)

(** Converts an ityp to a string *)
let rec string_of_ityp : ityp -> string =
  let is_complex = function
    | IFun _ -> true
    | _ -> false
  in
  let pp_nested t =
    let s = string_of_ityp t in if is_complex t then "(" ^ s ^ ")" else s
  in
  function
  | IVar i -> Format.sprintf "X%d" i
  | IInt -> "Int"
  | IFun (t1, t2) -> pp_nested t1 ^ " -> " ^ string_of_ityp t2
  | IList t -> Format.sprintf "List[%s]" (pp_nested t)

(** Typing environment module *)
module Env = Map.Make (String) ;;
type env = ityp Env.t

(** Type inference context. Tracks the typing environment, the generated
   constraints, and a counter for fresh variables.*)
module Context = struct
  type t = Context of env * (ityp * ityp) list * int

  (** The empty type inference context *)
  let empty = Context (Env.empty, [], 0)

  (** Returns the typing environment *)
  let env (Context (e, _, _)) = e

  (** Returns the list of constraints *)
  let cons (Context (_, c, _)) = c

  (** Return the next fresh variable (number) and the updated context *)
  let mk_fresh_var (Context (e, c, i)) = (i, Context (e, c, i + 1))

  (** Modify the environment with the given function, returning the updated context *)
  let modify_env f (Context (e, c, i)) = Context (f e, c, i)

  (** Add the equation t1 = t2 to the context, returning the updated context *)
  let add_eqn t1 t2 (Context (e, c, i)) = Context (e, (t1, t2) :: c, i)
end

(** Generate constraints for type inference. Returns the type (or type variable) *)
let rec gen_cons (ctx : Context.t) (e : expr) : ityp * Context.t =
  try
    match e with
    | (* CT-Int *)
      NumLit _ -> (IInt, ctx)
    | (* CT-Var *)
      Var x -> begin match Env.find_opt x (Context.env ctx) with
        | Some t -> (t, ctx)
        | None -> ty_err ("Unbound variable " ^ x)
      end
    | (* CT-Arith, CT-Ineq *)
      Binop (e1, (Add | Sub | Mul | Gt | Lt | And | Or), e2) ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let ctx3 = ctx2 |> Context.add_eqn t1 IInt
                 |> Context.add_eqn t2 IInt
      in
      (IInt, ctx3)
    | (* CT-Eq *)
      Binop (e1, Eq, e2) ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let ctx3 = ctx2 |> Context.add_eqn t1 t2
      in
      (IInt, ctx3)

    | (* CT-If *)
      IfThenElse (e1,e2,e3) ->
      let (t1, ctx1) = gen_cons ctx e1 in 
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let (t3, ctx3) = gen_cons ctx2 e3 in
      let ctx4 = ctx3 |> Context.add_eqn t1 IInt
                  |> Context.add_eqn t2 t3
      in
      (t2, ctx4)  

    | (* CT-Lambda *)
      Lambda (s, (t:typ option),ed) ->
      begin match t with
        | Some t1 -> (let ityp_t1= ityp_of_typ t1 in 
                      let ctx1=Context.modify_env (Env.add s ityp_t1) ctx in 
                      let (t2,ctx2) = gen_cons ctx1 ed in 
                      (IFun (ityp_t1,t2) , ctx2)
                      )
        | None -> (let (x,ctx1)= Context.mk_fresh_var ctx in
                    let ityp_x= IVar x in
                    let ctx2 = Context.modify_env (Env.add s ityp_x) ctx1 in
                    let (t2,ctx3) = gen_cons ctx2 ed in
                    (IFun (ityp_x,t2) , ctx3)
                    )
      end
    | (* CT-App *)
      App (e1, e2) ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let (temp_x1, ctx3) = Context.mk_fresh_var ctx2 in
      let (temp_x2, ctx4) = Context.mk_fresh_var ctx3 in 
      let x1= IVar temp_x1 in
      let x2= IVar temp_x2 in
      let ctx5=ctx4 |> Context.add_eqn t1 (IFun (x1, x2))
                    |> Context.add_eqn t2 x1 
      in
      (x2, ctx5) 

    | (* CT-Fix *)
      Fix ed -> 
      let (temp_x, ctx1) = Context.mk_fresh_var ctx in
      let x= IVar temp_x in
      let (t,ctx2) = gen_cons ctx1 ed in
      let ctx3=ctx2 |> Context.add_eqn t (IFun (x,x))
      in
      (x,ctx3)

    | (* CT-Let *) 
      LetBind (s, (t:typ option), e1, e2) -> 
      begin match t with
        | Some t0 -> (let ityp_t0= ityp_of_typ t0 in
                      let (t1, ctx1) = gen_cons ctx e1 in
                      let ctx2 = Context.modify_env (Env.add s ityp_t0) ctx1 in 
                      let (t2, ctx3) = gen_cons ctx2 e2 in
                      let ctx4= ctx3 |> Context.add_eqn ityp_t0 t1
                      in
                      (t2, ctx4)
                      )
        | None -> (let (temp_x, ctx1) = Context.mk_fresh_var ctx in
                   let x=IVar temp_x in
                   let (t1,ctx2) = gen_cons ctx1 e1 in
                   let ctx3 = Context.modify_env (Env.add s x) ctx2 in
                   let (t2, ctx4) = gen_cons ctx3 e2 in
                   let ctx5= ctx4 |> Context.add_eqn x t1
                   in
                   (t2, ctx5)
                  )
      end
    | (* CT-Nil *)
      ListNil (t: typ option) -> 
      begin match t with
        | Some t0 -> (let x=ityp_of_typ t0 in
                      (IList (x), ctx))
        | None -> (let (temp_x, ctx1) = Context.mk_fresh_var ctx in
                   let x= IVar temp_x in
                   (IList (x), ctx1))
      end
    | (* CT-Cons *)
      ListCons (e1, e2) ->
      let (t1,ctx1) = gen_cons ctx e1 in
      let (t2,ctx2) = gen_cons ctx1 e2 in
      let ctx3= ctx2 |> Context.add_eqn t2 (IList (t1))
      in
      ((IList (t1)), ctx3)
    | (* CT-IsNil *)
      ListIsNil ed ->
      let (t,ctx1) = gen_cons ctx ed in
      let (temp_x, ctx2)= Context.mk_fresh_var ctx1 in
      let x=IVar temp_x in
      let ctx3= ctx2 |> Context.add_eqn t (IList (x))
      in
      (IInt, ctx3)

    | (* CT-Head *)
      ListHead ed -> 
      let (t,ctx1) = gen_cons ctx ed in
      let (temp_x, ctx2)= Context.mk_fresh_var ctx1 in
      let x=IVar temp_x in
      let ctx3= ctx2 |> Context.add_eqn t (IList (x))
      in
      (x, ctx3)

    | (* CT-Tail *)
      ListTail ed ->
      let (t,ctx1) = gen_cons ctx ed in
      let (temp_x, ctx2)= Context.mk_fresh_var ctx1 in
      let x=IVar temp_x in
      let ctx3= ctx2 |> Context.add_eqn t (IList (x))
      in
      ((IList (x)), ctx3)
  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)

(** Module for type variable substitution *)
module Sub = Map.Make (Int)
type sub = ityp Sub.t

(** Module for set of integers *)
module IntSet = Set.Make (Int)

(** Find free type variables in a given type *)
let rec free_vars = function
  | IVar i -> IntSet.singleton i
  | IInt -> IntSet.empty
  | IList t -> free_vars t
  | IFun (t1, t2) -> IntSet.union (free_vars t1) (free_vars t2)

(** Apply the type substitution to the given type. *)
let rec sub_ityp sub = function
  | IInt -> IInt
  | IVar i -> begin match Sub.find_opt i sub with
      | None -> IVar i
      | Some t -> t
    end
  | IList t' -> IList (sub_ityp sub t')
  | IFun (t1, t2) -> IFun (sub_ityp sub t1, sub_ityp sub t2)

(** Apply the type substitution to each constraint in the given list of
   constraints. *)
let sub_cons sub cons =
  List.map (fun (t1, t2) -> (sub_ityp sub t1, sub_ityp sub t2)) cons

(** Unification algorithm that computes a type substitution that solves the
   constraints, or raises a Type_error if there is no solution. *)
let rec unify_helper (cons : (ityp * ityp) list) : sub = 
  try
    match cons with
    | [] -> Sub.empty
    | (t1,t2) :: c1 -> (match t1, t2 with
                        | t1, t2 when (t1=t2) -> unify_helper c1
                        | IVar x, t2 when ((IntSet.mem x (free_vars t2))=false) ->
                          (let temp_sub=Sub.singleton x t2 in
                           let c2= sub_cons temp_sub c1 in
                           let cur_sub= unify_helper c2 in
                           Sub.add x (sub_ityp cur_sub t2) cur_sub)

                        | t1, IVar x when ((IntSet.mem x (free_vars t1))=false) ->
                          (let temp_sub=Sub.singleton x t1 in
                           let c2= sub_cons temp_sub c1 in
                           let cur_sub= unify_helper c2 in
                           Sub.add x (sub_ityp cur_sub t1) cur_sub)

                        | IFun (t11,t12), IFun (t21,t22) ->
                          (let c2=(t11,t21) :: (t12,t22) :: c1 in
                           unify_helper c2
                          )
                        | IList t11, IList t12 -> 
                          (
                            let c2= (t11,t12)::c1 in
                            unify_helper c2
                          )
                        | _ -> ty_err "unify_helper inside failure"
                        )
  with
  | Type_error msg -> ty_err (msg ^ "unify_helper try failure")

let unify cons =
  let sub0 = unify_helper cons in
  let bound_vars = List.map fst (Sub.bindings sub0) in
  (* Replaces all type variables in the inferred type with concrete types,
     allowing only "free" type variables. This works by substituting type
     variables until a fixed point is reached (which is guaranteed since the
     occurs check ensures that the solution graph is a DAG). *)
  let rec apply_sub sub' =
    let ty_vars = List.fold_left IntSet.union IntSet.empty
        (List.map (fun (_, t) -> free_vars t) (Sub.bindings sub'))
    in
    if IntSet.is_empty (IntSet.inter ty_vars (IntSet.of_list bound_vars))
    then sub'
    else apply_sub (Sub.map (sub_ityp sub') sub')
  in
  apply_sub sub0

(** Hindley-Milner type inference *)
let type_infer (e : expr) : ityp * sub =
  let (ty, ctx) = gen_cons Context.empty e in
  (*
  Format.printf "Generated constraints:\n%!";
  List.iter (fun (t1, t2) -> Format.printf "%s = %s\n%!" (string_of_ityp t1) (string_of_ityp t2))
    (Context.cons ctx) ;
  *)
  let sub = unify (Context.cons ctx) in
  (*
  Format.printf "Computed substitution:\n%!";
  List.iter (fun (i, t) -> Format.printf "%d => %s\n%!" i (string_of_ityp t))
    (Sub.bindings sub) ;
  *)
  (sub_ityp sub ty, sub)