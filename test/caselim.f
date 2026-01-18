type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

program
      let null [a] (xs: list a): bool =
        match xs return bool with
        | Nil [_] {} -> True {}
        | Cons [_] {_; _} -> False {}
        end
      in
      let tail [a] (xs: list a): list a =
        match xs return list a with
        | Nil [_] {} -> Nil [a] {}
        | Cons [_] {_; xs} -> xs
        end
      in
      let ifte [a] (b: bool)(e1: a)(e2: a): a =
        match b return a with
        | True {} -> e1
        | False {} -> e2
        end
      in
      let caselim [a] (xs: list a)(default: list a): list a =
        ifte [list a] (null [a] xs) default (tail [a] xs)
      in
      caselim
