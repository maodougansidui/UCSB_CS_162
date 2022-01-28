fun helper with x, y =
    if x > y
    then x
    else y
in
fun max with ls=
    if isnil ls
    then 0
    else helper (!ls) (max (#ls))
in
fun largest_elems with l =
    if isnil l
    then Nil
    else if isnil (!l)
    then largest_elems (#l)
    else (max (!l)) @ (largest_elems (#l)) 
in
largest_elems

