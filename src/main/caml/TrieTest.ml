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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

module Rnd = Random.State

module IntHashtbl = Hashtbl.Make(struct
  type t = int
  let equal = (=)
  let hash i = i
end)

module StrSet = SetUtil.StringSet

module StrSetTrie = Trie.Make
(struct
  type t = int
  let zero = 0
  let prev x = x - 1
  let compare = compare
  let stringer = Stringer.int
end)
(struct
  type value_t = string
  type stored_t = StrSet.t
  let zero_value = StrSet.empty
  let combine = StrSet.union
  let promote = StrSet.singleton
  let equal = StrSet.equal
  let compare = StrSet.compare
  let stringer out s =
    Stringer.list Stringer.string out (StrSet.elements s)
end)

let str_printer s = "\n\t" ^ (Stringer.s Stringer.string s)

let str_set_printer ss = "(" ^ (String.concat ", " (StrSet.elements ss)) ^ ")"

let assert_str_set_equal =
  assert_equal ~printer:str_set_printer ~cmp:StrSet.equal

let seed = [| 0x8bcd63b0; 0x7c0ca93f; 0x401aa936 |]
let rnd = Rnd.make seed

let test_fixture = "Trie" >::: [
  "single_level_multi_trie" >:: (fun _ ->
    (* Test equivalence between a trie and a treemap. *)
    for runs = 9 downto 0 do
      ignore runs;
      let t = StrSetTrie.make () in
      let m = IntHashtbl.create 100 in

      for i = 0 to 99 do
        ignore i;
        let s = string_of_int (Rnd.int rnd 128) in
        let lt = Rnd.int rnd 160 in
        let rt = lt + 1 + Rnd.int rnd 32 in

        List.iter
          (fun c -> StrSetTrie.set c s)
          (StrSetTrie.get_all t lt rt);
        for j = lt to rt-1 do
          if not (IntHashtbl.mem m j) then
            IntHashtbl.replace m j StrSet.empty;
          IntHashtbl.replace m j (StrSet.add s (IntHashtbl.find m j))
        done;

        for j = 0 to 199 do
          let c = StrSetTrie.get t j in
          let a = match c with
            | Some t -> StrSetTrie.value t
            | _ -> StrSet.empty in
          let b = (
            if IntHashtbl.mem m j then
              IntHashtbl.find m j
            else
              StrSet.empty) in
          assert_str_set_equal ~msg:(Printf.sprintf "j=%d" j) b a
        done;

        StrSetTrie.simplify t;

        for j = 0 to 199 do
          let c = StrSetTrie.get t j in
          let a = match c with
            | Some t -> StrSetTrie.value t
            | _ -> StrSet.empty in
          let b = (
            if IntHashtbl.mem m j then
              IntHashtbl.find m j
            else
              StrSet.empty) in
          assert_str_set_equal ~msg:(Printf.sprintf "j=%d" j) b a
        done
      done
    done
  );
  "copy" >:: (fun _ ->
    let t = StrSetTrie.make() in
    StrSetTrie.set t "root";
    List.iter
      (fun sub -> StrSetTrie.set sub "a")
      (StrSetTrie.get_all t 0 10);
    List.iter
      (fun sub ->
        StrSetTrie.set sub "b";
        List.iter (fun sub -> StrSetTrie.set sub "c")
          (StrSetTrie.get_all sub 5 15))
      (StrSetTrie.get_all t 10 20);
    List.iter
      (fun sub -> StrSetTrie.set sub "c")
      (StrSetTrie.get_all t 20 30);

    let copy = StrSetTrie.copy t in

    assert_equal ~printer:str_printer
      (Stringer.s StrSetTrie.stringer t)
      (Stringer.s StrSetTrie.stringer copy);
  );
]

let () = TestHarnessWrapper.register_test test_fixture
