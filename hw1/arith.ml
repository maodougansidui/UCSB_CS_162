(** Arithmetic expressions *)
type expr
  = (** A natural number constant *)
    Nat of int
  | (** A variable of unknown value *)
    Var of string
  | (** Addition *)
    Add of expr * expr
  | (** Multiplication *)
    Mul of expr * expr

(** Example: 5 + (1 * 2) *)
let example_expr = Add(Nat(5), Mul(Nat(1), Nat(2)))

(** Simplify an arithmetic expression *)

let rec simplify (e : expr) : expr =
        match e with 
        | Nat _ | Var _ as exp -> exp
        | Add (exp1, exp2) -> (let result=Add (simplify exp1, simplify exp2) in
                                match result with
                                | Add (Nat a, Nat b) -> Nat (a+b)
                                | Add (Nat a, b) when a=0 -> b
                                | Add (a, Nat b) when b=0 ->a
                                | _ -> result)
        | Mul (exp1, exp2) -> (let result=Mul (simplify exp1, simplify exp2) in
                                match result with 
                                | Mul (Nat a, Nat b) -> Nat(a*b)
                                | Mul (Nat a, b) when a=0 -> Nat 0
                                | Mul (a, Nat b) when b=0 -> Nat 0
                                | Mul (Nat a, b) when a=1 -> b
                                | Mul (a, Nat b) when b=1 -> a 
                                | _ -> result);;
