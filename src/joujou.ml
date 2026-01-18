open Printf
open Print

(* ------------------------------------------------------------------------- *)

(* Parse the command line. *)

let filename =
  ref None

let please_echo =
  ref false

let please_optimize =
  ref false

let usage =
  sprintf "Usage: %s <options> <filename>\n" Sys.argv.(0)

let () =
  Arg.parse (Arg.align [
    "--echo", Arg.Set please_echo, " Prior to typechecking, echo intermediary steps";
    "--optimize", Arg.Set please_optimize, " Typecheck, optimize, and display the optimized program";
    "--case-of-case", Arg.Set Simplifier.optimize_caseofcase, " Enable case-of-case optimization";
  ]) (fun name -> filename := Some name) usage

let filename =
  match !filename with
  | Some filename ->
      filename
  | None ->
      fprintf stderr "%s%!" usage;
      exit 1

(* -------------------------------------------------------------------------- *)

(* Printing a syntax tree in an intermediate language (for debugging). *)

let print_delimiter () =
  Printf.eprintf "----------------------------------------";
  Printf.eprintf "----------------------------------------\n"

let dump (phase : string) (show : 'term -> string) (t : 'term) =
  if !please_echo then begin
    print_delimiter();
    Printf.eprintf "%s:\n\n%s\n\n%!" phase (show t)
  end;
  t

(* ------------------------------------------------------------------------- *)

(* Read the file; lex; parse; internalize; typecheck; simplify; print. *)

let read filename : Syntax.program =
  let lexbuf = LexerUtil.open_in filename in
  try
    Parser.program Lexer.main lexbuf
  with Parser.Error ->
    Error.errorb lexbuf "Syntax error.\n"

let simplify filename prog =
  ignore (Typecheck.run prog);
  let prog = Typecheck.petrify prog in
  if !please_optimize then
    Simplifier.program filename prog
  else
    prog

let output prog =
  Printf.printf "%s" (print_program prog)

let process filename =
  filename
  |> read
  |> dump "AST" (Format.asprintf "%a" Syntax.pp_program)
  |> Internalize.program
  |> dump "Internalized" print_program
  |> simplify filename
  |> dump "Simplified" print_program
  |> output

(* -------------------------------------------------------------------------- *)

(* The main program. *)

let () =
  (* If we ask for the case-of-case optimization,
     then we are asking for the simplifier to be run *)
  if ! Simplifier.optimize_caseofcase then
    please_optimize := true;

  (* Go! *)
  process filename
