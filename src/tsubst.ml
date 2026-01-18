open Atom
open Types

(* ------------------------------------------------------------------------- *)

(* Type substitution maps type variables to types. *)

type tsubst = ftype AtomMap.t

let empty = AtomMap.empty

let bind : atom -> ftype -> tsubst -> tsubst =
  AtomMap.add

let binds xts tenv =
  List.fold_left (fun tenv (x, ty) ->
    bind x ty tenv
  ) tenv xts

let binds_tycon ty1 ty2 tsubst =
  match ty1, ty2 with
  | TyCon (tc1, args1), TyCon (tc2, args2) when Atom.equal tc1 tc2 ->
     let args1 = List.map (fun (TyFreeVar x) -> x) args1 in
     binds (List.combine args1 args2) tsubst
  | _ -> assert false

let rec apply tsubst typ =
  match typ with
  | TyFreeVar a when AtomMap.mem a tsubst ->
     let typ = AtomMap.find a tsubst in
     apply tsubst typ

  | TyFreeVar a -> TyFreeVar a

  | TyBoundVar i -> TyBoundVar i

  | TyArrow (domain, codomain) ->
     TyArrow (apply tsubst domain,
              apply tsubst codomain)

  | TyCon (tc, args) ->
     TyCon (tc, List.map (apply tsubst) args)

  | TyForall body ->
      let a = Atom.fresh (hint body) in
      let ta = TyFreeVar a in
      TyForall (abstract a (apply tsubst (fill body ta)))

  (* Some constructs are supposed to appear in type schemes, not in types,
     so we do not deal with them here. *)

  | TyTuple _ ->
      assert false (* should not happen *)

(* We could avoid 3 traversals here. *)
let equal tsubst ty1 ty2 =
  Types.equal (apply tsubst ty1) (apply tsubst ty2)
