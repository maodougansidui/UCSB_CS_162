fun take with n, l =
    if n=0 || (isnil l)
    then Nil
    else (!l) @ (take (n-1) (#l))
in
take

