(*
  Copyright 2012 Google, Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 *)

include DisableGenericCompare

let (test_flags        : (string * string) list),
    (** Flags that may be examined by test-cases.
        Some tests dump intermediate results like logs and DOT files for
        additional debugging.
        These flags use the prefix ["--test."]. *)
    (test_runner_flags : string list),
    (** Flags meant for OUnit2. *)
    (test_args         : string list),
    (** Flags meant for the test filter. *)
    (is_verbose        : bool)
    (** Controlled by [-v]. *)
= begin
  let is_verbose            = ref false in
  let test_flags_rev        = ref [] in
  let test_runner_flags_rev = ref [] in
  let rec parse_flags argv = match argv with
    | []                                  -> []
    (* -- causes later arguments to be treated as not flags *)
    | "--"::tl                            -> tl
    (* First non-flag argument stops flag interpretation *)
    | hd::_ when str_eq hd "" || not (chr_eq hd.[0] '-') -> argv
    | flag::tl ->
      (* Double dash flags have a value.
         Either --flag=value or --flag value. *)
      let rest =
        if str_eq flag "-v" then begin
          is_verbose := true;
          tl
        end else if StringUtil.starts_with flag "-test." then begin
          test_flags_rev := (flag, "")::!test_flags_rev;
          tl
        end else if StringUtil.starts_with flag "--test." then begin
          let key, value, rest = match StringUtil.index_of flag "=" with
            | None -> (match tl with
                | value::rest -> flag, value, rest
                | []          -> flag, "",    [])
            | Some eq ->
              let ep1 = eq+1 in
              let key   = String.sub flag 0 eq in
              let value = String.sub flag ep1 (String.length flag - ep1) in
              key, value, tl in
          test_flags_rev := (key, value)::!test_flags_rev;
          rest
        end else begin
          test_runner_flags_rev := flag::!test_runner_flags_rev;
          tl
        end in
      parse_flags rest in
  let test_args = parse_flags (List.tl (Array.to_list Sys.argv)) in
  (
    List.rev !test_flags_rev,
    List.rev !test_runner_flags_rev,
    test_args,
    !is_verbose
  )
end

let find_test_flags flag_name =
  List.map snd (List.filter (fun (n, _) -> str_eq n flag_name) test_flags)

let is_verbose () = is_verbose

(* HACK HACK: Hide all of Sys.argv not meant for the OUnit2 test runner to
   stop it from dumping --help output every time we ask it be verbose.
   TODO: If OUnit2.run_test_tt_main gets a better way of accepting argv, use it.
*)
let () = begin
  let rec rewrite_argv testrunner_argv i =
    if i >= 1 (* Skip program name *) then
      let arg, testrunner_argv' = match testrunner_argv with
        | []     -> "-no-output-junit-file", []
        | hd::tl -> hd, tl in
      Sys.argv.(i) <- arg;
      rewrite_argv (testrunner_argv') (i-1)
    else
      assert (is_empty testrunner_argv) in
  rewrite_argv (List.rev test_runner_flags) ((Array.length Sys.argv) - 1)
end

