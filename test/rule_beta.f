(* TODO: improve example so that inline is disabled? *)
program
      fun [a][b] (e: a -> b)(v: a): b =
        (fun (x : a) = e x) v
