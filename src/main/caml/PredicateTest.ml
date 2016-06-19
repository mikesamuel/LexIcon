(*
  Copyright 2014 Google, Inc.

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

module Atom = struct
  type t = | V of string | Nand of t list
  type 'a context = unit

  let empty_context = ()

  let nand ls = Nand ls

  let decompose ~of_nand ~of_atom x = match x with
    | V    _  -> of_atom x
    | Nand ls -> of_nand ls

  let invert_atom _ _ = None

  let rec repr_stringer out x = match x with
    | Nand ls -> Stringer.ctor "Nand" (Stringer.list repr_stringer) out ls
    | V    s  -> out s

  let atom_stringer _ _ = repr_stringer

  let inv_atom_stringer _ _ = None

  let and_op = "et"
  let or_op  = "ou"
  let not_op = "pas"
  let false_keyword = "faux"
  let true_keyword  = "vrai"
end

module P = Predicate.Make (Atom)

type atom_t = Atom.t = | V of string | Nand of Atom.t list

let () = TestHarnessWrapper.register_test (
  "Predicate" >::: [
    "as_conjunction" >:: (fun _ ->
      let printer = Stringer.s P.stringer in

      let assert_a_j input want =
        let got = P.as_conjunction input in
        let smart_printer =
          if str_eq (printer want) (printer got) then
            Stringer.s Atom.repr_stringer
          else
            printer in
        assert_equal ~msg:(printer input) ~printer:smart_printer want got in

      let x = V "x" in
      let y = V "y" in
      let z = V "z" in

      P.(
        assert_a_j _true                    _true;
        assert_a_j _false                   (Nand [Nand [Nand []]]);
        assert_a_j (_and [x])               (Nand [Nand [x]]);
        assert_a_j (_and [x; y; z])         (_and [x; y; z]);
        assert_a_j (_and [_and [x; y]; z])  (_and [x; y; z]);
        assert_a_j (_or  [x; y])            (Nand [Nand [_or [x; y]]]);
        assert_a_j (_or  [x; Nand [y]])
          (Nand [Nand [Nand [Nand [x]; Nand [Nand [y]]]]]);
      );
    )
  ])
