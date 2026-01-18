type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

type maybe a
data constructor Nothing : forall a. {} -> maybe a
data constructor Just : forall a. { a } -> maybe a

program
      let caseofcase [a][b][c]
             (v: list a)
             (e1: maybe b)(e2: a -> list a -> maybe b)
             (big1: c)(big2: b -> c): c =
        match (match v return maybe b with
               | Nil [a] {} -> e1
               | Cons [a] {x;xs} -> e2 x xs
               end) return c with
        | Nothing [b] {} -> big1
        | Just [b] {x} -> big2 x
        end
      in
      caseofcase
