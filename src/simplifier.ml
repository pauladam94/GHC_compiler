open Atom
open Terms
open Types
open Tsubst

(* ------------------------------------------------------------------------- *)

(* Specification of the simplifier. *)

(* The simplifier consumes a typechecked (and petrified) program and
   produces a typecheck-able program of the same type. It works by
   iterating a simplification pass [max_iter] number of times.

   In the present implementation, we chose cautiousness at the expense
   of brevity and efficiency. As a result, we *systematically* dump
   the result of each intermediate simplification pass into a file and
   (see function [linter]), re-load this file and re-typecheck it,
   thus producing traceable errors in case of a bug in the simplifier.

   Because we are going to juggle with partly-optimized terms, we use
   typing to distinguish "terms that have already been simplified"
   from "terms that still need to be simplified". A term that *will*
   undergo simplification must carry some typing metadata, produced by
   the typecheker: we will therefore use the type [fterm] to represent
   those. A term that *has been through* simplification does not carry
   typing metadata we can trust: we will therefore use the type
   [pre_fterm] to represent those, systematically populating the
   metadata with [reset].

   Watch out: we have not been this careful with types (e.g., which
   may appear in [TeTyAnnot])! So, you will have to be careful to
   instantiate types that were obtained from an [fterm] before
   inserting them in a [pre_fterm].
 *)

(* ------------------------------------------------------------------------- *)

(* Flag set at runtime to indicate whether case-of-case
   transformations must be applied or not. *)

let optimize_caseofcase = ref false

(* ------------------------------------------------------------------------- *)

(* Term substitution. *)

(* As we go along simplifying a term, we may decide to substitute an
   actual term for some term variable, ie. performing "inlining". We
   do so by maintaining a substitution mapping (unique) variables to
   terms.
 *)

module Subst = struct

  type t =
    pre_fterm AtomMap.t

  let empty = AtomMap.empty

  let bind = AtomMap.add

  let lookup k default env = 
    try
      AtomMap.find k env
    with Not_found -> default

end

(* ------------------------------------------------------------------------- *)

(* Scoping annotation. *)

(* This type definition is purely informational: it (verbosely)
   indicates that its argument is to be manipulated under a term
   substitution [subst] and type substitution [tsubst].

   You will have been warned:
     - when manipulating a [scoped] value, make sure to discharge
       these substitutions before producing [pre_fterm]s; and
     - when creating a [scoped] value, make sure to associate the
       right substitutions to it 
 *)

type 'a scoped = 
  Scope of Subst.t * tsubst * 'a

(* ------------------------------------------------------------------------- *)

(* Simplification context. *)

(* The job of the simplifier is essentially to traverse [fterm]s,
   building an evaluation context and recognizing valid
   transformations. Evaluation contexts are represented through a
   typical zipper construction.
 *)

type context = 
  Nil of ftype
| CtxtApp of (fterm * application_info) scoped * context
| CtxtTyApp of (ftype * type_application_info) scoped * context
| CtxtMatch of (ftype * clause list * ftype_info) scoped * context

(* [type_of_hole E] computes the type [ty] of the hole in `E[ _ : ty ]` *)

let type_of_hole = function
  | Nil ty -> ty
  | CtxtApp (Scope (_, tsubst, (_, info)), _) -> 
     Tsubst.apply tsubst (type_from_application_info info)
  | CtxtTyApp (Scope (_, tsubst, (_, info)), _) -> 
     Tsubst.apply tsubst (type_from_type_application_info info)
  | CtxtMatch (Scope (_, tsubst, (_, _, info)), _) -> 
     Tsubst.apply tsubst info

(* [type_of_cont E] computes the type [ty] of the whole continuation `E[ _ ] : ty` *)
let rec type_of_cont = function
  | Nil ty -> ty
  | CtxtApp (_, args) -> type_of_cont args
  | CtxtTyApp (_, args) -> type_of_cont args
  | CtxtMatch (_, args) -> type_of_cont args

(* ------------------------------------------------------------------------- *)

(* Simplification routine. *)

(* [simplify (Scope (subst, tsubst, term))] yields a [pre_fterm]
   [pterm] such that
     [term [subst] [tsubst]] is equivalent to [pterm].
 *)
let rec simplify: fterm scoped -> pre_fterm = function
  | Scope (_, tsubst, term) as input -> 
     let typ = Tsubst.apply tsubst (Typecheck.type_of term) in
     simplify1 (Nil typ) input

(* [simplify1 E (Scope (subst, tsubst, term))] yields a [pre_fterm]
   [pterm] such that

      [ E[ term [subst] [tsubst] ] ] is equivalent to [pterm].

   where [E [ t ]] consists in zipping the evaluation context [E]
   (represented by [args]) onto to [t] (this is achieved with [apply],
   defined below). *)
and simplify1
          (args: context)
          (Scope (subst, tsubst, term): fterm scoped): pre_fterm =
  match term with
  (* 0. go through noise *)
  | TeTyAnnot (term, _) ->
     (* Update type annotation *)
     let typ = type_of_cont args in
     let term = simplify1 args (Scope (subst, tsubst, term)) in
     TeTyAnnot (term, typ)
  | TeLoc (loc, term) ->  
     (* Drop locations, as they become meaningless *)
     simplify1 args (Scope (subst, tsubst, term))

  (* 1. Build up the evaluation context E[_] in args *)
  | TeApp (term1, term2, info) ->
     let args = CtxtApp (Scope (subst, tsubst, (term2, info)), args) in
     simplify1 args (Scope (subst, tsubst, term1))
  | TeTyApp (term1, type2, info) ->
     let args = CtxtTyApp (Scope (subst, tsubst, (type2, info)), args) in
     simplify1 args (Scope (subst, tsubst, term1))
  | TeMatch (scrutinee, result, clauses, info) ->
     let args = CtxtMatch (Scope (subst, tsubst, (result, clauses, info)), args) in
     simplify1 args (Scope (subst, tsubst, scrutinee))


  (* 2. Contract the context as much as possible *)
  (*    rule (\beta), (\beta_\tau), (\case), etc. *)

  | _ when false -> failwith "Simplify the context here!"

  | _ ->
     (* 3. Structural rules *)
     let term = simplify2 (Scope (subst, tsubst, term)) in
     (* 4. Discharge (and optimize) context *)
     apply term args

(* [simplify2 (Scope (subst, tsubst, term))] yields a [pre_fterm]
   [pterm] such that
     [term [subst] [tsubst]] is equivalent to [pterm].

   It focuses on structural optimization rules.
 *)

and simplify2
        (Scope (subst, tsubst, term): fterm scoped): pre_fterm =
  match term with
  | TeLet (x, term1, term2) ->
     let term1 = simplify (Scope (subst, tsubst, term1)) in
     let term2 = simplify (Scope (subst, tsubst, term2)) in
       TeLet (x, term1, term2)

  | TeVar (x, info) -> 
     Subst.lookup x (TeVar (x, reset ())) subst

  | TeAbs (x, domain, body) ->
     let domain = Tsubst.apply tsubst domain in
     let body = simplify (Scope (subst, tsubst, body)) in
     TeAbs (x, domain, body)

  | TeTyAbs (a, body) ->
     let body = simplify (Scope (subst, tsubst, body)) in
     TeTyAbs (a, body)

  | TeData (dc, tys, fields, info) ->
     let tys = List.map (Tsubst.apply tsubst) tys in
     let local_simplify t = simplify (Scope (subst, tsubst, t)) in
     let fields = List.map local_simplify fields in
     TeData (dc, tys, fields, reset ())


  | _ -> assert false


(* [simplify_clause scrutinee clause] propagates simplification to the
   term within the clause while maintaining the term and type
   substitutions. *)

and simplify_clause
    (scrutinee: pre_fterm)
    (Scope (subst, tsubst, clause): clause scoped)
    : pre_clause =
  match clause with
  | Clause (pattern, term) ->
     let pattern = simplify_pattern pattern in
     let term = simplify (Scope (subst, tsubst, term)) in
     Clause (pattern, term)

(* [simplify_pattern p] merely has to drop the useless metadata info. *)
and simplify_pattern (PatData (loc, dc, tyvars, tevars, _)) = 
  PatData (loc, dc, tyvars, tevars, reset ())


(* [apply t args] flattens out the evaluation context, represented by
   [args], around the term [t]. Since we are dealing with scoped
   constructs, we have to carefully and discharge the term and type
   substitutions. This is also where one can spot commuting
   conversions. *)
and apply (t: pre_fterm): context -> pre_fterm = function
  | Nil _ -> t
  | CtxtApp (Scope (subst, tsubst, (a, _)), args) -> 
     let a = simplify (Scope (subst, tsubst, a)) in
     apply (TeApp (t, a, reset ())) args
  | CtxtTyApp (Scope (subst, tsubst, (ty, _)), args) -> 
     let ty = Tsubst.apply tsubst ty in
     apply (TeTyApp (t, ty, reset ())) args
  | CtxtMatch (Scope (subst, tsubst, (ty, cases, info)), args) -> 
        let ty = Tsubst.apply tsubst ty in
        let local_sclauses case = simplify_clause t (Scope (subst, tsubst, case)) in
        let cases = List.map local_sclauses cases in
        apply (TeMatch (t, ty, cases, reset ())) args


(* ------------------------------------------------------------------------- *)

(* Simplification loop. *)

(* The [linter] is responsible for the dirty business of turning an
   optimized term [t] back into a full program sitting on your
   hard-drive. We then fire up the usual typechecker so as to get
   located and traceable error messages. *)

let linter filename typ tctable dctable t =
  let p = Prog (tctable, dctable, t) in
  let outchan = open_out filename in
  let out = Format.formatter_of_out_channel outchan in
  Format.fprintf out "%s%!" (Print.print_program p);
  close_out outchan;
  let lexbuf = LexerUtil.open_in filename in
  let readback = Parser.program Lexer.main lexbuf in
  let p' = Internalize.program readback in
  let _, typ' = Typecheck.run p' in
  assert (Types.equal typ typ');
  Unix.unlink filename

(* [simplify_many] runs the simplification pass [max_iter] number of
   times, irrespectively of whether something happened or not. *)
let simplify_many outfile typ tctable dctable =
  let base = Filename.remove_extension outfile in
  let max_iter = 10 in
  let rec go n (t: fterm): fterm =
    if n >= max_iter then t
    else
      begin
        (* Simplify *)
        let t = simplify (Scope (Subst.empty, Tsubst.empty, t)) in

        (* Lint *)
        let filename = Printf.sprintf "%s_%d.fj.dump" base n in
        linter filename typ tctable dctable t;
        
        (* Typecheck (again) and petrify the simplified term *)
        ignore (Typecheck.run (Prog (tctable, dctable, t)));
        let t = Typecheck.petrify_fterm t in

        (* Start again *)
        go (n+1) t
      end
  in
  go 0

(* [program outfile prog] optimizes the program [prog] while, in case
   of a type error, dumping the intermediary steps in
   [outfile_*.fj.dump]. *)
let program outfile (Prog (tctable, dctable, t)) = 
  let typ = Typecheck.type_of t in
  let t = simplify_many outfile typ tctable dctable t in
  Prog (tctable, dctable, t)
