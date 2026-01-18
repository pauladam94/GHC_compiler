type c
data constructor C : {} -> c

program
  fun [a] [b] (v : b) = 
    let j [b] (x : b) : c =  C {} in
    j [a] v
