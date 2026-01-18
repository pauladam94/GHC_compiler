open Terms
open Types

(* Configuration knob for [Main] *)
val optimize_caseofcase : bool ref

(* Whole program simplification. The first argument is the name of the
   input file, used to produce similarly named debug dump in case of a
   type error. *)
val program : string -> program -> program
