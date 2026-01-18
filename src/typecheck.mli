open Terms
open Types

(* The type-checker checks that a complete program is well-typed. *)

(* Furthermore, the type-checker records typing information in various
   nodes, as explained in [Terms]. This is done by writing the
   references that exist at these nodes. *)

(* The type-checker returns the inferred type of the program, together
   with an export environment that allows printing this type if
   desired. *)

val run: pre_program -> Export.env * ftype

(* Petrifaction turns the dynamic ([ref]) metadata produced by the
   typechecker into static metadata (no [ref]). In doing so, it makes
   sure that every piece of metadata has indeed been filled by the
   typechecker. *)

val petrify : pre_program -> program
val petrify_fterm : pre_fterm -> fterm

(* [type_of t] is a constant time operation that exploits the metadata
   produced by the typechecker to return the type of any given
   petrified term. *)

val type_of: fterm -> ftype
