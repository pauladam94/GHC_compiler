type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type string
data constructor Gotcha : {} -> string

type int
data constructor Four : {} -> int

program
    let test (b: bool): int =
      join j : string = Gotcha {} in
      match b return int with
      | True {} -> jump j {} : int
      | False {} -> Four {}
      end
    in
    test
