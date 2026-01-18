open Atom
open Types

(* Type substitution maps type variables to types. *)

type tsubst

val empty : tsubst

val bind : atom -> ftype -> tsubst -> tsubst
val binds : (atom * ftype) list -> tsubst -> tsubst

(* [binds_tycon typ1 typ2 tsubst] expects two types that are expected
   to be fully-applied type constructors and binds the variables of
   [typ1] with the instances of [typ2] into [tsubst] *)

val binds_tycon : ftype -> ftype -> tsubst -> tsubst

(* [apply tsubst typ] discharges the type substitution [tsubst] into [typ]. *)

val apply: tsubst -> ftype -> ftype

(* [equal tsubst typ1 typ2] tells whether [typ1] and [typ2] are equal
   under the type substitution [tsubst]. *)

val equal: tsubst -> ftype -> ftype -> bool
