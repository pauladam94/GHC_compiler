type bool

program
fun (left : bool) (right : bool) (test : bool -> bool -> bool) : bool = 
  test left right
