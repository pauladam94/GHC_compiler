type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

program
      fun [a] (v1 v2: a): a =
        match True {} return a with
        | True {} -> v1
        | False {} -> v2
        end
