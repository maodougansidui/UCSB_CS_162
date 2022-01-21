(** Reverses a list. *)
let rec cat (xs: 'a list) (ys: 'a list) : 'a list =
        match xs with
        | [] -> ys
        | h::t -> h::(cat t ys);;

let rec rev (xs: 'a list) : 'a list =
        match xs with
        | [] -> []
        | h::t -> cat (rev t) [h];;
