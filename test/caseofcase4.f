type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

program
      let test [a][b][c] (b: bool)
               (e1 e2: list b)
               (big1 : c)(big2 : b -> list b -> c) =
        let f (as: list b): c =
          match as return c with
          | Nil [_] {} -> big1
          | Cons [_] {a; as} -> big2 a as
          end
        in
        f (match b return list b with
           | True {} -> e1
           | False {} -> e2
           end)
      in
      test
