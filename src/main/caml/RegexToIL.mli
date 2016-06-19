(*
  Copyright 2013 Google, Inc.

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

(** Compile {!Regex.t}s to the intermediate language. *)

val chars_to_open_ranges :
  CodeUnitKind.t -> CodeUnit.Range.Set.t -> IL.OpenRange.Set.t
(** [chars_to_open_ranges parse_kind r] *)

val translate : 'm Regex.t -> IL.match_kind -> CodeUnitKind.t -> 'm IL.fn
(** [translate re mk parse_kind] is a function that takes
    (an input cursor, a limit, and a pointer to a match result)
    and which succeeds when [re] matches between the cursor and the limit
    with the given anchoredness.

    Regardless of whether the regex matches or not, the caller will not
    observe any change to the input cursor.

    @param parse_kind the type of code-unit expected on the input buffer.
*)
