open Atom

(* ------------------------------------------------------------------------- *)

(* Types. *)

type ftype =
  | TyBoundVar of int
  | TyFreeVar of atom
  | TyArrow of ftype * ftype
  | TyForall of ftype_context
  | TyCon of atom * ftype list
  | TyTuple of ftype list

(* Internally, a type context is just a type in which the de Bruijn index 0 is
   allowed to occur. *)

(* A type context also records an identifier, whose sole purpose is to serve as
   a hint when pretty-printing. It can otherwise be ignored. *)

and ftype_context =
    Identifier.identifier * ftype

[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)

(* Equality. *)

let rec equal ty1 ty2 =
  match ty1, ty2 with
  | TyBoundVar i1, TyBoundVar i2 ->
      i1 = i2
  | TyFreeVar a1, TyFreeVar a2 ->
      Atom.equal a1 a2
  | TyArrow (domain1, codomain1), TyArrow (domain2, codomain2) ->
      equal domain1 domain2 && equal codomain1 codomain2
  | TyForall (_, body1), TyForall (_, body2) ->
      equal body1 body2
  | TyCon (tc1, tys1), TyCon (tc2, tys2) ->
      begin try
        let id1 = Identifier.name (Atom.identifier tc1) in
        let id2 = Identifier.name (Atom.identifier tc2) in
        (* compare type constructors by their absolute name *)
        (* TODO: this enables comparing types across different files
           but this is rather inelegant *)
	id1 =  id2 && 
          List.for_all2 equal tys1 tys2
      with Invalid_argument _ ->
	assert false (* arity error: should not happen *)
      end
  | TyTuple tys1, TyTuple tys2 ->
      begin try
	List.for_all2 equal tys1 tys2
      with Invalid_argument _ ->
	false (* arity mismatch: these are valid but distinct tuple types *)
      end
  | _, _ ->
      false

(* ------------------------------------------------------------------------- *)

(* Context creation. *)

let rec abstract a i ty =
  match ty with
  | TyFreeVar b when Atom.equal a b ->
      TyBoundVar i
  | TyFreeVar _
  | TyBoundVar _ ->
      ty
  | TyArrow (domain, codomain) ->
      TyArrow (abstract a i domain, abstract a i codomain)
  | TyForall (hint, body) ->
      TyForall (hint, abstract a (i+1) body)
  | TyCon (tc, tys) ->
      TyCon (tc, List.map (abstract a i) tys)
  | TyTuple tys ->
      TyTuple (List.map (abstract a i) tys)

let abstract a ty =
  let hint = Atom.identifier a in
  hint, abstract a 0 ty

(* ------------------------------------------------------------------------- *)

(* Context elimination. *)

let rec fill i ty c =
  match c with
  | TyBoundVar j when i = j ->
      ty
  | TyBoundVar _
  | TyFreeVar _ ->
      c
  | TyArrow (domain, codomain) ->
      TyArrow (fill i ty domain, fill i ty codomain)
  | TyForall (hint, body) ->
      TyForall (hint, fill (i+1) ty body)
  | TyCon (tc, tys) ->
      TyCon (tc, List.map (fill i ty) tys)
  | TyTuple tys ->
      TyTuple (List.map (fill i ty) tys)

let fill (_, c) ty =
  fill 0 ty c

(* ------------------------------------------------------------------------- *)

(* Hints. *)

let hint (hint, _) =
  hint

(* ------------------------------------------------------------------------- *)

(* Typing environments map term variables to types. *)

type tenv =
    ftype AtomMap.t

let empty : tenv =
  AtomMap.empty

let lookup : atom -> tenv -> ftype =
  fun x tenv ->
    try
      AtomMap.find x tenv
    with Not_found ->
      assert false (* should not happen, if there is a binding of [x] in [tenv] *)

let bind : atom -> ftype -> tenv -> tenv =
  AtomMap.add

let binds xts tenv =
  List.fold_left (fun tenv (x, ty) ->
    bind x ty tenv
  ) tenv xts


(* Join-typing environments map term variables to types. *)

type jenv =
  (atom list * ftype list) AtomMap.t

let jempty : jenv = AtomMap.empty

let jlookup : atom -> jenv -> atom list * ftype list =
  fun x jenv ->
    try
      AtomMap.find x jenv
    with Not_found ->
      assert false (* should not happen, if there is a binding of [x] in [tenv] *)

let jbind (j: atom)(a: atom list)(tys: ftype list): jenv -> jenv =
  AtomMap.add j (a, tys)

(* ------------------------------------------------------------------------- *)

(* Extra utilities. *)

let rec count_foralls accu = function
  | TyForall (_, body) ->
      count_foralls (accu+1) body
  | _ ->
      accu

let count_foralls ty =
  count_foralls 0 ty
