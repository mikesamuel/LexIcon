(* Usage: ocaml make.ml [<ModuleToBuild>.{byte,native}] *)

#load "unix.cma";;

let args = match Array.to_list Sys.argv with
  | []      -> failwith "no program on argv"
  | _::[]   -> ["AllTests.native"]
  | _::argv -> argv

let library_paths = [
  (* Hard-coded library paths for when ocamlfind gets widgy *)
]

(* If the -p flag is first in the argument list, then replace it
   with a host of flags that enable profiling via ocamlprof. *)
let args, c_opt_flags, l_opt_flags =
  match args with
    | "-p"::rest ->
      (* Treat the targets as debug and profile targets. *)
      "-tags"::"debug,profile"
      (* Use profiling versions of the compilers which instrument
         various flow control constructs with entry counters. *)
      ::"-ocamlc"::"ocamlcp"
      ::"-ocamlopt"::"ocamloptp"
      ::rest,
      (* Instrument all available points. *)
      ["-P"; "a"],
      (* Link with a wrapper that dumps profiling data if program
         exits noramlly. *)
      ["-p"]
    | _ -> args, [], []

let standard_flags = [
  "-use-ocamlfind";
  "-pkgs"; "str,unix,ounit,ocamlgraph,num";
  "-cflags"; (String.concat "," (
    [
      "-g";
      "-w"; "+a-4";
      "-warn-error"; "+a-4";
      "-safe-string";
    ]
    @ c_opt_flags
    @ library_paths));
  "-lflags"; (String.concat "," (
    [
      "-g";
    ]
    @ l_opt_flags
    @ library_paths));
]

let build_command = "ocamlbuild"

let argv = build_command::(standard_flags @ args)

let _ = begin
  Printf.printf "%s\n" (String.concat " \\\n\t" argv);
  flush stdout;
  Unix.execvp build_command (Array.of_list argv);
end;;
