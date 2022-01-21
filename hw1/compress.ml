(** Removes all consecutive duplicate elements from a list. *)

let rec compress (xs: 'a list) : 'a list =
        match xs with
        | [] -> []
        | h::[] -> [h]
        | h::(h1::t) -> if h=h1 then compress (h1::t) 
                        else h:: (compress (h1::t));;
