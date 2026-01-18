program
      fun [a] (e: a)(three: a)(plus : a -> a -> a)  = 
        let y = e in
        (fun (x : a)(y : a) = plus x y) (plus y three)
