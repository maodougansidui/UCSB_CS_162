(** Returns the maximum of an integer list if the list is non-empty,
  * or None otherwise.
  *)

let larger (x:int option) (y:int option): int option= 
        if x=None then y
        else if y=None then x
        else if x > y then x
        else y;;

let rec max (ns: int list) : int option =
        match ns with 
        [] -> None
        | h::t -> larger (Some h) (max t);;


