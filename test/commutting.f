type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

program
      let ifte [a] (b: bool)(e1: a)(e2: a): a =
        match b return a with
        | True {} -> e1
        | False {} -> e2
        end
      in
      let bad [a] (e1: bool)(e2: bool)(e3: bool)(big4: a)(big5: a): a =
        ifte [a] (ifte [bool] e1 e2 e3) big4 big5
      in
      bad
