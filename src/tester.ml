open Printf
open Print
open Error
open Store
open Exn

(* Colors *)
let violet () = print_to_buffer "\027[1;35m"
let orange () = print_to_buffer "\027[1;35m"
let blue () = print_to_buffer "\027[1;34m"
let red () = print_to_buffer "\027[1;31m"
let green () = print_to_buffer "\027[1;32m"
let reset () = print_to_buffer "\027[0m"

(* Some printing helpers *)

(** Custom assert with colors. If then assert fail we dump the current
    dump_store *)
let my_assert (name : string) (success : bool) : unit =
  flush_buffer (not success);
  if success then (
    green ();
    print_to_buffer "[Passed] ";
    reset ();
    print_to_buffer (sprintf "%s\n" name))
  else (
    red ();
    print_to_buffer "[Failed] ";
    reset ();
    print_to_buffer (sprintf "%s\n" name));
  flush_buffer true

(** Store output of the dump during the process of a file *)

(* Parse the command line. *)

let optimize_caseofcase = ref false
let stuff = ref ""
let usage = sprintf "Usage: %s <options>\n" Sys.argv.(0)

let () =
  Arg.parse
    (Arg.align
       [
         ( "--case-of-case",
           Arg.Set optimize_caseofcase,
           " Enable case-of-case optimization" );
       ])
    (* here filename is modified to contain the filename *)
    (fun name -> stuff := name)
    usage

(* ------------------------------------------------------------------------- *)

(* Read the file; lex; parse; internalize; typecheck; simplify; print. *)

let read (filename : string) : Syntax.program =
  let lexbuf = LexerUtil.open_in filename in
  try Parser.program Lexer.main lexbuf
  with Parser.Error -> Error.errorb lexbuf "Syntax error.\n"

let simplify (please_optimize : bool) (filename : string)
    (prog : Terms.pre_program) =
  ignore (Typecheck.run prog);
  let prog = Typecheck.petrify prog in
  if please_optimize then Simplifier.program !optimize_caseofcase filename prog
  else prog

let output prog : string = print_program prog

(* -------------------------------------------------------------------------- *)

(** remove_whitespace from a string *)
let remove_whitespace (s : string) : string =
  s |> String.to_seq
  |> Seq.filter (fun c ->
      not
        (Char.equal c ' ' || Char.equal c '\n' || Char.equal c '\t'
       || Char.equal c '\r'))
  |> String.of_seq

(** Returns a filename without it's extension *)
let without_extension (s : string) : string =
  let sep = '.' in
  let split = String.split_on_char sep s in
  let remove_end = split |> List.rev |> List.tl |> List.rev in
  String.concat (String.make 1 sep) remove_end

(** Read an entire file into a string *)
let read_file (filename : string) : string =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(** remove_whitespace from a string *)
let remove_whitespace (s : string) : string =
  String.concat "" (String.split_on_char ' ' s)

(** Do the diff between two string without taking into account spacing *)
let diff_string (expected : string) (actual : string) : bool =
  remove_whitespace expected = remove_whitespace actual

let multiple_space (len : int) : string =
  let s = ref "" in
  for _ = 0 to len do
    s := sprintf "%s%s" !s " "
  done;
  !s

let diff (expected : string) (actual : string) : unit =
  let left = String.split_on_char '\n' expected |> Array.of_list in
  let right = String.split_on_char '\n' actual |> Array.of_list in
  let max_len_expected =
    Array.fold_left
      (fun acc s -> if String.length s > acc then String.length s else acc)
      0 left
  in

  let should_fail = ref false in

  let print_left_right l r =
    print_to_buffer
      (sprintf "%s%s | %s\n" l
         (multiple_space (max_len_expected - String.length l))
         r)
  in
  let min_len = Int.min (Array.length left) (Array.length right) in
  for i = 0 to min_len - 1 do
    if diff_string left.(i) right.(i) then green ()
    else (
      red ();
      should_fail := true);
    print_left_right left.(i) right.(i);
    reset ()
  done;
  if !should_fail then raise (DiffError "Error on the diff")

(** Do the diff between two files *)
let diff_files (filename1 : string) (filename2 : string) : bool =
  let s1 = read_file filename1 in
  let s2 = read_file filename2 in
  diff_string s1 s2

(** Get files with a given extension always sorted in the same way *)
let files_with_ext directory extension =
  if Sys.file_exists directory then
    Sys.readdir directory |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f extension)
    |> List.map (Filename.concat directory)
    (* Sort the filenames to have a deterministic output *)
    |> List.sort (fun f1 f2 ->
        Int.compare
          (f2 |> read_file |> String.length)
          (f1 |> read_file |> String.length))
  else []

(* -------------------------------------------------------------------------- *)

(* Test Task 1 *)

(** Checks if the file typecheck or not depeding on [should_fail] *)
let test1 (should_fail : bool) (filename : string) =
  try
    let please_optimize = false in
    blue ();
    print_to_buffer (sprintf "[TESTING %s]" filename);
    reset ();
    let _ =
      filename |> read
      |> dump
           (Printf.sprintf "AST : %s" filename)
           (Format.asprintf "%a" Syntax.pp_program)
      |> Internalize.program
      |> dump (Printf.sprintf "Internalized : %s" filename) print_program
      |> simplify please_optimize filename
      |> dump (Printf.sprintf "Simplified %s" filename) print_program
      |> output
    in
    my_assert filename (not should_fail)
  with e -> (
    my_assert filename should_fail;
    match e with
    | TypeCheckError s -> ()
    | Failure s | Invalid_argument s | UnboundAtom s -> raise e)

(* -------------------------------------------------------------------------- *)

(* Test Task 2 *)

let test2 (should_fail : bool) (filename_spec : string) =
  let please_optimize = true in
  let filename = sprintf "%s.f" (without_extension filename_spec) in
  try
    blue ();
    print_to_buffer (sprintf "[TESTING %s]" filename);
    reset ();

    let expected = read_file filename_spec in
    let simplified_program =
      filename |> read
      |> dump
           (Printf.sprintf "AST : %s" filename)
           (Format.asprintf "%a" Syntax.pp_program)
      |> Internalize.program
      |> dump (Printf.sprintf "Internalized : %s" filename) print_program
      |> simplify please_optimize filename
      |> dump (Printf.sprintf "Simplified %s" filename) print_program
      |> output
    in

    diff simplified_program expected;
    my_assert filename (not should_fail)
  with e -> (
    my_assert filename_spec should_fail;
    match e with
    | DiffError s -> printf "%s\n" s
    | TypeCheckError s | Failure s | Invalid_argument s | UnboundAtom s ->
        raise e
    | _ -> raise e)

(* The main program. *)

let () =
  violet ();
  print_to_buffer "\n>>>>>>>>>> TEST 1 - Type Checking <<<<<<<<<\n";
  reset ();
  flush_buffer true;

  files_with_ext "../test/" ".f" |> List.iter (test1 false);
  files_with_ext "../test/bad/" ".f" |> List.iter (test1 true);
  violet ();
  print_to_buffer
    "\n\
     >>>>>>>>>> TEST 2 - Program Simplification \
     (inline)(drop)(case)(beta)(beta_T) <<<<<<<<<\n";
  reset ();
  flush_buffer true;

  files_with_ext "../test" ".spec2" |> List.iter (test2 false)

(* print_string ">>>>>>>>>> TEST 3 <<<<<<<<<\n" *)

(* todo add again this
  (* If we ask for the case-of-case optimization,
     then we are asking for the simplifier to be run *)
  if !Simplifier.optimize_caseofcase then please_optimize := true;

  (* Go! *)
  process filename
    *)
