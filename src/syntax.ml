(* This module defines the abstract syntax of Core, as produced
   by the parser. *)

(* This version of the syntax is still concrete, in the sense that
   names are identifiers. A more abstract version of the syntax is
   defined by the module [Types] and [Terms]. The conversion is
   performed by [Internalize]. *)

open Identifier
open Error

(* ------------------------------------------------------------------------- *)

(* Sorts. *)

(* We have four sorts of identifiers: term variables, type variables,
   data constructors, and type constructors. Each sort defines an
   independent namespace. *)

let term_sort : sort = (0, "term")
let type_sort : sort = (1, "type")
let data_sort : sort = (3, "data constructor")
let typecon_sort : sort = (4, "type constructor")

(* ------------------------------------------------------------------------- *)

(* Types. *)

type ftype =
  | SynTyArrow of ftype * ftype
      (* T -> T *)
  | SynTyForall of identifier * ftype
      (* forall a . T *)
  | SynTyVarOrTyCon of (string * (Lexing.position[@opaque]) * (Lexing.position[@opaque])) * ftype list
      (* a *)
      (* tc T ... T *)
      (* our syntax is ambiguous: type variables and type constructors of arity
	 0 look alike. The ambiguity is kept here and is resolved during the
	 import phase. *)

[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)

(* Type schemes for data constructors. *)

type scheme =
  | SynScheme of identifier list * ftype list * identifier * ftype list
      (* forall a ... a. { T; ... ; T } -> tc T ... T *)

[@@deriving show { with_path = false }]  

type signature_item =
  | SynType of identifier * identifier list
      (* type tc a ... a *)
  | SynDatacon of identifier * scheme
      (* datacon K : S *)

[@@deriving show { with_path = false }]  

(* ------------------------------------------------------------------------- *)

(* Terms. *)

(* The parser supports a few derived forms in addition to the primitive forms
   that are listed here. The derived forms include:

   Anonymous functions with multiple type and term arguments:

     fun [ a ... a ] (x : T) ... (x : T) = t

   Anonymous functions:

     fun f [ a ... a ] (x : T) ... (x : T) : T = t

   Named functions:

     let f [ a ... a ] (x : T) ... (x : T) = t in t

   These derived forms are desugared into the primitive forms listed here. *)

type fterm =
  | SynTeVar of identifier
      (* x *)
  | SynTeAbs of identifier * ftype * fterm
      (* fun (x : T) = t *)
  | SynTeApp of fterm * fterm
      (* t t *)
  | SynTeLet of identifier * fterm * fterm
      (* let x = t in t *)
  | SynTeTyAbs of identifier * fterm
      (* fun [ a ] = t *)
  | SynTeTyApp of fterm * ftype
      (* t [ T ] *)
  | SynTeData of identifier * ftype list * fterm list
      (* K [ T ... T ] { t; ...; t } *)
  | SynTeTyAnnot of fterm * ftype
      (* (t : T) *)
  | SynTeMatch of fterm * ftype * clause list
      (* match t return T with clause ... clause end *)
  | SynTeLoc of location * fterm
      (* t *)
      (* the parser generates [SynTeLoc] nodes to keep track of locations
	 within the source code. *)

and clause =
  | SynClause of pattern * fterm
      (* p -> t *)

and pattern =
  | SynPatData of location * identifier * identifier list * identifier list
      (* K [ a ... a ] { x; ...; x } *)

[@@deriving show { with_path = false }]  

(* ------------------------------------------------------------------------- *)

(* Programs. *)

type program =
  SynProg of signature_item list * fterm

[@@deriving show { with_path = false }]  
