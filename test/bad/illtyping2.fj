type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type int
data constructor One : {} -> int
data constructor Two : {} -> int
data constructor Three : {} -> int

program
      let ex (plus: int -> int -> int): int =
        join j (x: int): int = plus x (One {}) in
        (plus (Two {}) (One {})) (Three {})
      in
      ex
