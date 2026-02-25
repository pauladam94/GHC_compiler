open Printf
open Lexing

type location = Lexing.position * Lexing.position

let pp_location oc _ = Format.fprintf oc "<>"
let dummy : location = (Lexing.dummy_pos, Lexing.dummy_pos)

(** A dummy location. *)
let is_dummy ((pos1, pos2) : location) : bool =
  pos1 == Lexing.dummy_pos && pos2 == Lexing.dummy_pos

(** [override loc1 loc2] returns [loc2], unless [loc2] is a dummy location,
   in which case it returns [loc1]. *)
let override (loc1 : location) (loc2 : location) : location =
  if is_dummy loc2 then loc1 else loc2

let print_location (pos1, pos2) : string =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in
  (* intentionally [pos1.pos_bol] *)
  sprintf "File \"%s\", line %d, characters %d-%d:\n" file line char1 char2
(* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)
