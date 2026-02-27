open Atom
open Types

(* Type substitution maps type variables to types. *)

type tsubst

val empty : tsubst
val bind : atom -> ftype -> tsubst -> tsubst
val binds : (atom * ftype) list -> tsubst -> tsubst

val binds_tycon : ftype -> ftype -> tsubst -> tsubst

val apply : tsubst -> ftype -> ftype

val equal : tsubst -> ftype -> ftype -> bool
