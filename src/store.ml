(** Storing the print of the current test if the test eventually fails then we
    print it else we restore it to nothing *)
let error_store = ref ""

(** Store the prints progressively *)
let print_to_buffer (s : string) =
  error_store := Printf.sprintf "%s%s" !error_store s

let print_delimiter () =
  print_to_buffer (Printf.sprintf "----------------------------------------");
  print_to_buffer (Printf.sprintf "----------------------------------------\n")

let dump (phase : string) (show : 'a -> string) (t : 'a) =
  print_delimiter ();
  print_to_buffer (Printf.sprintf "%s:\n\n%s\n\n%!" phase (show t));
  t

(** dump the current dump_store and restore to empty *)
let flush_buffer (should_print : bool) =
  if should_print then Printf.printf "%s" !error_store;
  error_store := ""
