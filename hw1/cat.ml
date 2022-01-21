(** Concatenates two lists. *)

let rec cat (xs: 'a list) (ys: 'a list) : 'a list =
        match xs with
        | [] -> ys
        | h::t -> h::(cat t ys);;
