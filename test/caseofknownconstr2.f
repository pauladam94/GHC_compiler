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
fun [a] (xs : list a) = 
  match xs return bool with
  | Nil [_] {} ->
     False {}
  | Cons [a] { x; _ } ->
      match Just [a] { x } return bool with
      | Nothing [_] {} ->
          False {}
      | Just [_] { _ } ->
          True {}
      end
  end
