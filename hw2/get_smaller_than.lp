fun get_smaller_than with n, l =
    if isnil l
    then Nil
    else if (!l) < n 
    then (!l) @ (get_smaller_than (n) (#l))
    else (get_smaller_than (n) (#l))
in
get_smaller_than