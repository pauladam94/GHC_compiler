(* TODO: improve example so that drop is disabled? *)
program
      fun [a][b](v: a)(c: a -> b): b =
        let x : a = v in c x
