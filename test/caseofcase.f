type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type maybe a
data constructor Nothing : forall a. {} -> maybe a
data constructor Just : forall a. { a } -> maybe a

type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

program
      let isNothing [a] (ma: maybe a): bool =
        match ma return bool with
        | Nothing [ _ ] {} -> False {}
        | Just [ _ ] { _ } -> True {}
        end
      in
      let mhead [a] (xs: list a) : maybe a =
        match xs return maybe a with
        | Nil [a] {} -> Nothing [a] {}
        | Cons [a] {x ; _} -> Just [a] { x }
        end
      in
      let null [a] (xs: list a) : bool =
        isNothing [a] (mhead [a] xs)
      in
      null
