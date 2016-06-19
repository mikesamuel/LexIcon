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

(**
   Collects, filter, and on exit, runs OUnit tests.

   Other test modules call {!register_test} to register an ounit test.

   On exit, this module uses {!TestConfig.test_args} to filter tests
   and then runs the tests that pass the filters.

   A test filter is a test path separated by colons like
   [FooTest:12:foo_is_not_bar].

   A test filter element may be [*] to match any path element.

   If the first element of a test path or test filter path is ["all_tests"]
   then it is ignored.
   Test filter elements that are strings of decimal digits are also ignored,
   and the index of a test in a test list is ignored.

   Since index elements are ignored, the test filter path [Foo:1:bar] is
   semantically equivalent to [Foo:bar] but not to [Foo:*:bar] since the
   [*] must match a significant path element.

   If a test filter starts with [-] then it is inverted, so
   [Foo:123:bar_test] matches the test name [Foo:123:bar_test] where [123]
   can be any counting number and [-Foo:bar_test] explicitly rejects that
   test.

   If the test filter path with the fewest elements is not negated and the
   test filter path [*] is not present, then [-*] is implied.
*)

exception No_tests

module ExceptionPrinters = ExceptionPrinters

let tests = ref []   (** Accumulated tests defined by modules *)

(* Report stacktraces in test failures *)
let () = Printexc.record_backtrace true

(** Register tests that can be run via [run_registered_tests()] *)
let register_test (test : OUnitTest.test) =
  tests := test::!tests

module TestFilter : sig
  type t

  val make : string list -> t
  val filter : t -> OUnitTest.test -> OUnitTest.test
  val stringer : t Stringer.t
end = struct
  module TestPathMap = MapUtil.StringMap

  type t =
    | SkipAll
    | RunAll
    | Path of t TestPathMap.t * t

  let rec split_on_colon name = match StringUtil.index_of_char name ':' with
    | None   -> [name]
    | Some i ->
      (String.sub name 0 i)
      ::(split_on_colon (String.sub name (i+1) (String.length name-i-1)))

  let is_ignorable_part index name_part =
    (str_eq name_part "all_tests" && index = 0)
    || (StringUtil.for_all (fun c -> '0' <=% c && c <=% '9') name_part
        && String.length name_part <> 0)

  let rec stringer out f = match f with
    | RunAll -> out "RunAll"
    | SkipAll -> out "SkipAll"
    | Path (trie, def) ->
      Stringer.ctor "Path"
        (Stringer.tup2 (TestPathMap.stringer stringer) stringer)
        out (trie, def)

  let make paths =
    (* Parse paths by splitting on colon and dropping ignorable elements.
       Test paths have the form

       Name0:1:Name2:3:Name4

       where the indices are indices into the lists in OUnit.TestList ctors,
       but we ignore them since the printed indices post-filtering don't match
       the input indices anyway so are just confusing.

       We also ignore

       AllTest:*:

       since the all_tests is just a global grouping node and
       adds no information.
    *)
    let leaves_and_path_parts = List.map
      (fun path ->
        let leaf, path_suffix =
          if chr_eq path.[0] '-' then
            SkipAll, String.sub path 1 (String.length path - 1)
          else
            RunAll, path in
        let path_parts = ListUtil.filteri
          (fun i x -> not (is_ignorable_part i x))
          (split_on_colon path_suffix) in
        (leaf, path_parts))
      paths in
    (* If the least-specific path is a positive path, then assume -* as a path
       since the user is specifying the tests they want, not the tests they
       don't. *)
    let most_specific_leaf_opt, _ = List.fold_left
      (fun ((_, length) as best) (leaf, path_parts) -> match path_parts with
        | ["*"] -> Some leaf, ~-1
        | _     ->
          let path_len = List.length path_parts in
          if length > path_len then
            Some leaf, path_len
          else
            best)
      (None, max_int) leaves_and_path_parts in
    let global_bias = match most_specific_leaf_opt with
      | Some RunAll -> SkipAll
      | _           -> RunAll in
    (* Build the top-level tree. *)
    List.fold_left
      (fun f (leaf, path_parts) ->
        let rec insert f path_parts = match path_parts with
          | [] -> (match f with
              | SkipAll
              | RunAll         -> leaf
              | Path (trie, _) -> Path (trie, leaf))
          | path_part::path_parts_tl -> (match f with
              | SkipAll | RunAll ->
                Path (
                  TestPathMap.singleton path_part
                    (insert global_bias path_parts_tl),
                  f)
              | Path (trie, def) ->
                let child =
                  if TestPathMap.mem path_part trie then
                    TestPathMap.find path_part trie
                  else
                    RunAll in
                Path (
                  TestPathMap.add path_part (insert child path_parts_tl) trie,
                  def)
          ) in
        insert f path_parts)
      global_bias leaves_and_path_parts

  let filter f test =
    (* apply returns the empty test list to indicate that a test was filtered
       out, and tries to flatten and propagate test lists preserving labels
       and other metadata only on significant nodes.
       It makes no effort to preserve test indices as those will never survive
       filtering. *)
    let rec apply f test = match f with
      | RunAll           -> test
      | SkipAll          -> OUnitTest.TestList []
      | Path (trie, default) -> (match test with
          | OUnitTest.TestLabel (lbl, t) ->
            let sub_filter = match (TestPathMap.find_opt lbl trie,
                                    TestPathMap.find_opt "*" trie) with
              | Some x, _
              | None,   Some x -> x
              | None,   None   -> default in
            (match apply sub_filter t with
              | OUnitTest.TestList [] as empty -> empty
              | t'                             -> OUnitTest.TestLabel (lbl, t'))
          | OUnitTest.TestList ls ->
            OUnitTest.TestList (
              List.rev (
                List.fold_left
                  (fun ls_rev t -> match apply f t with
                    | OUnitTest.TestList ls -> List.rev_append ls ls_rev
                    | t'                    -> t'::ls_rev)
                  [] ls
              )
            )
          | OUnitTest.TestCase _ -> apply default test
      ) in
    match test with
      | OUnitTest.TestLabel ("all_tests" as lbl, t) -> (match apply f t with
          | OUnitTest.TestList [] as empty -> empty
          | t'                             -> OUnitTest.TestLabel(lbl, t')
      )
      | _                                           -> apply f test

end

let run_registered_tests () =
  let all_tests = List.rev !tests in
  tests := [];

  let filter_paths = match TestConfig.test_args with
    | [] -> None
    | _  -> Some TestConfig.test_args in

  let test_harness = match all_tests with
    | []  -> raise No_tests
    | [t] -> t
    | _   -> OUnitTest.(>:::) "all_tests" all_tests in

  let filtered = match filter_paths with
    | None                   -> test_harness
    | Some test_filter_paths ->
      let test_filter = TestFilter.make test_filter_paths in
      TestFilter.filter test_filter test_harness in

  ignore (OUnit2.run_test_tt_main filtered)

(* Exit on Ctrl-C. *)
(*let () = begin
  Sys.catch_break true;
  let flush_and_abort _ =
    prerr_string "aborted by signal\n";
    flush stderr;
    flush stdout;
    exit ~-1 in
  Sys.set_signal Sys.sigint  (Sys.Signal_handle flush_and_abort);
  Sys.set_signal Sys.sigquit (Sys.Signal_handle flush_and_abort);
end*)

(* Run any remaining tests before exit. *)
let () = at_exit (fun () -> match !tests with
  | [] -> ()
  | _ -> run_registered_tests())
