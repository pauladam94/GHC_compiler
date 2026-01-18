type t

type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type maybe a
data constructor Nothing : forall a. {} -> maybe a
data constructor Just : forall a. { a } -> maybe a

program
      match Just [bool] { False {} } return maybe bool with
      | Nothing [a] {} -> Just [a] { True {} }
      | Just [a] { v } -> Just [a] { v }
      end
