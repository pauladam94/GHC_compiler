open Printf
open Atom
open Types
open Terms
open Symbols
open Print
open Typerr

module TS = Tsubst

(* ------------------------------------------------------------------------- *)

(* Specification of the type-checker. *)

(* The core of the typechecker is made up of two mutually recursive
   functions. [infer] infers a type and returns it; [check] infers a
   type, checks that it is equal to the expected type, and returns
   nothing. *)

(* The type [ty] that is produced by [infer] should be correct, that
   is, the typing judgement [tenv |- term : ty [tsubst] ] should hold. *)

(* The function [check] should be correct, that is, if it succeeds,
   then the typing judgement [tenv |- term : expected [tsubst] ] holds. *)

(* ------------------------------------------------------------------------- *)

(* The type-checker. *)

let rec infer              (* [infer] expects... *)
    (p : pre_program)      (* a program, which provides information about type & data constructors; *)
    (xenv : Export.env)    (* a pretty-printer environment, for printing types; *)
    (loc : Error.location) (* a location, for reporting errors; *)
    (tsubst : TS.tsubst)   (* a type substitution; *)
    (tenv : tenv)          (* a typing environment; *)
    (jenv : jenv)          (* a join-typing environment [unused until Task 3]; *)
    (term : pre_fterm)     (* a term; *)
    : ftype =              (* ...and returns a type. *)

  match term with

  | TeVar (x, info) ->
      
      (* Look up [x] in the typing environment. *)

      (* This lookup cannot fail, because [Internalize] checks that all
	 identifiers are properly bound when it replaces identifiers with
	 atoms. *)

     let ty = lookup x tenv in
     info := Some ty;
     ty

  | TeAbs (x, domain, body) ->

      (* Extend the typing environment. *)

      let outside_tenv = tenv in
      let tenv = bind x domain tenv in

      (* Typecheck the function body, and build a function type. *)

      let codomain = infer p xenv loc tsubst tenv jempty body in
      let fty = TyArrow (domain, codomain) in

      (* Return the type of the function. *)

      fty

  | TeApp (term1, term2, info) ->

      (* Typecheck the left sub-term, and check that its type is an arrow.
	 Record its domain and codomain, for use by the defunctionalization
	 phase. Check the right sub-term against the domain of the
	 arrow. Return the codomain of the arrow. *)

      let domain, codomain = deconstruct_arrow xenv loc (infer p xenv loc tsubst tenv jenv term1) in
      info := Some { domain = domain; codomain = codomain };
      check p xenv tsubst tenv jenv term2 domain;
      codomain

  | _ ->
     failwith "TYPECHECKING IS NOT IMPLEMENTED YET!" (* do something here! *)

and check                  (* [check] expects... *)
    (p : pre_program)      (* a program, which provides information about type & data constructors; *)
    (xenv : Export.env)    (* a pretty-printer environment, for printing types; *)
    (tsubst : TS.tsubst)   (* a type substitution; *)
    (tenv : tenv)          (* a typing environment; *)
    (jenv : jenv)          (* a join-typing environment [unused until Task 3]; *)
    (term : pre_fterm)     (* a term; *)
    (expected : ftype)     (* an expected type; *)
    : unit =               (* ...and returns nothing. *)

  (* We bet that the term begins with a [TeLoc] constructor. This should be
     true because the parser inserts one such constructor between every two
     ordinary nodes. In fact, this is not quite true, because the parser
     sometimes expands syntactic sugar without creating intermediate [TeLoc]
     nodes. If you invoke [check] in reasonable places, it should just work. *)

  match term with
  | TeLoc (loc, term) ->

      let inferred = infer p xenv loc tsubst tenv jenv term in
      failwith "CHECK IS NOT COMPLETE YET!" (* do something here! *)

  | _ ->
     (* out of luck! We run in degraded mode, location will be wrong!
        This should only happen on simplified terms. *)
      let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
      check p xenv tsubst tenv jenv (TeLoc (dummy_loc, term)) expected


(* ------------------------------------------------------------------------- *)

(* [lookup_and_instantiate p xenv loc dc tys] finds the type scheme associated
   with the data constructor [dc] and applies it to the type arguments
   [tys]. *)

and lookup_and_instantiate p xenv loc dc tys =

  (* Find the type scheme associated with this data constructor. *)

  let dcty = type_scheme p dc in

  (* Check that an appropriate number of type arguments is passed. *)

  let expected = count_foralls dcty
  and found = List.length tys in
  if expected <> found then
    arity_mismatch xenv loc "data constructor" dc "type" expected found;

  (* Perform the type applications. *)

  List.fold_left (fun dcty ty ->
    fill (deconstruct_univ xenv loc dcty) ty (* cannot fail *)
  ) dcty tys

(* ------------------------------------------------------------------------- *)

(* [deconstruct_data_arrow xenv loc dc dcty found] expects that the
   universal quantifiers and equations in [dcty] have been eliminated,
   so what remains is an arrow type, whose domain is a tuple type.
   (This cannot fail, since all type schemes have this structure.)
   It checks that the arity of this arrow type is equal to [found].
   It returns the list [domains] and the type [codomain]. *)

and deconstruct_data_arrow xenv loc dc dcty found =

  (* What remains of [dcty] should now be an arrow type, whose domain
     is a tuple type. This cannot fail, since all type schemes have
     this structure. *)

  let domains, codomain =
    match dcty with
    | TyArrow (TyTuple domains, codomain) ->
	domains, codomain
    | _ ->
	assert false
  in

  (* Check that the term arguments are in the required number. *)

  let expected = List.length domains in
  if expected <> found then
    arity_mismatch xenv loc "data constructor" dc "term" expected found;

  domains, codomain

(* ------------------------------------------------------------------------- *)

(* [check_clause p xenv hyps tenv domain codomain clause] checks that [clause]
   implements a function of type [domain] to [codomain]. *)

and check_clause p xenv tsubst tenv jenv scrutinee result = function
  | Clause (PatData (loc, dc, tyvars, tevars, infos), term) ->

      (* Introduce the type arguments. This requires extending the pretty-
	 printing environment. *)

      let xenv = List.fold_left Export.bind xenv tyvars in

      (* Look up and instantiate the type scheme associated with this
	 data constructor. *)

      let typs = List.map (fun a -> TyFreeVar a) tyvars in
      let dcty = lookup_and_instantiate p xenv loc dc typs in

      (* What remains of [dcty] should now be an arrow type, whose domain
	 is a tuple type. Extract the domains and codomain. *)

      let domains, codomain =
	deconstruct_data_arrow xenv loc dc dcty (List.length tevars)
      in

      (* Bind the term variables. *)

      let tenv = binds (List.combine tevars domains) tenv in (* no arity error is possible *)

      infos := Some domains;

      (* Further assume an equation between the codomain of the data constructor
	 and the type of the scrutinee. *)

      let tsubst = TS.binds_tycon codomain scrutinee tsubst in

      (* Finally, check the body of the clause. *)

      check p xenv tsubst tenv jenv term result

(* ------------------------------------------------------------------------- *)

(* A complete program is typechecked within empty environments. *)

let run (Prog (tctable, dctable, term) as p : pre_program) =
  let xenv = Export.empty
  and loc = Error.dummy
  and tsubst = TS.empty
  and tenv = Types.empty
  and jenv = Types.jempty in
  let xenv = AtomMap.fold (fun tc _ xenv -> Export.bind xenv tc) tctable xenv in
  let xenv = AtomMap.fold (fun dc _ xenv -> Export.bind xenv dc) dctable xenv in
  let ty = infer p xenv loc tsubst tenv jenv term in
  xenv, ty

(* ------------------------------------------------------------------------- *)

(* Petrifaction amounts to forcing every metadata throughout. *) 

let petrify_fterm (term: pre_fterm): fterm =
  map__fterm force force force force force force term

let petrify (Prog (tctable, dctable, term)) =
  Prog (tctable, dctable, petrify_fterm term)
  
(* ------------------------------------------------------------------------- *)

(* [type_of t] reconstructs typing information from metadata and
   structural knowledge. *)

let rec type_of (term: fterm): ftype =
  match term with
  | TeVar (_, ty) -> ty
  | TeApp (term1, term2, info) -> info.codomain
  | TeTyApp (term, type1, info) -> fill info.gen type1
  | TeData (_, _, _, info) -> info

  | TeAbs (x, dom, body) -> TyArrow (dom, type_of body)
  | TeLet (_, term1, term2) -> type_of term2

  | TeTyAbs (a, term) ->
     TyForall (abstract a (type_of term))
  | TeMatch (_, ty, _, _) -> ty

  | TeTyAnnot (_, ty) -> ty
  | TeLoc (_, term) -> type_of term
