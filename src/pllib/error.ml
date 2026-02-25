open Printf
open Lexing
open Loc

let signaled = ref false

let atoms2locs atoms =
  List.map
    (fun a ->
      let id = Atom.identifier a in
      (Identifier.startp id, Identifier.endp id))
    atoms

let warning locs message =
  List.iter (fun loc -> printf "%s" (print_location loc)) locs;
  printf "%s%!" message

let signal locs message =
  warning locs message;
  signaled := true

let signala atoms message = signal (atoms2locs atoms) message

let error locs message =
  signal locs message;
  failwith (sprintf "%s" message)

let errora atoms message = error (atoms2locs atoms) message

let errorb lexbuf msg =
  error [ (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ] msg

let signaled () = if !signaled then exit 1
