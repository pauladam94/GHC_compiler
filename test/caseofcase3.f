type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

program
      let or_shortcut (x y: bool): bool =
        match x return bool with
        | True {} -> True {}
        | False {} -> y
        end
      in
      let caseofcase [a] (x y: bool)(big1: a)(big2: a): a =
        match (or_shortcut x y) return a with
        | True {} -> big1
        | False {} -> big2
        end
      in
      caseofcase
