type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type t
data constructor A : {} -> t
data constructor B : {} -> t
data constructor C : {} -> t


program
      let preserve (v: t)(big: bool -> bool): bool =
        match (let j (x: bool): bool = big x in
          match v return bool with
          | A {} -> j (True {})
          | B {} -> j (False {})
          | C {} -> True {}
          end) return bool with
        | True {} -> False {}
        | False {} -> True {}
        end
      in
      preserve
