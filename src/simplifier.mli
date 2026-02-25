open Terms
open Types

(* Whole program simplification. The first argument is the name of the
   input file, used to produce similarly named debug dump in case of a
   type error. *)
val program : bool -> string -> program -> program
