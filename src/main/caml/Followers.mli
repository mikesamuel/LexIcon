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


(**
   Computes a conservative (guaranteed super-set) follower-set for each node in
   a grammar, the set of characters (or the special end of file symbol) that can
   immediately follow a string that can be produced by forcing the grammar up to
   but not including that node.

   The grammar {[\[f\] \[o\] \[o\]? \[bar\]]} specifies the language
   [("fob" "foa" "fooa" "foob" "foar" "for")] so when numbering the position
   before nodes:
   {[
      (  "f"  "o"  (  "o" |  () )  [bar] )
      ^  ^    ^    ^  ^      ^     ^
      |  |    |    |  |      |     |
      0  1    2    3  4      5     6
   ]}
   {ul
     {li  0 \[f\]}
     {li  1 \[f\]}
     {li  2 \[o\]}
     {li  3 \[abor\]}
     {li  4 \[o\]}
     {li  5 \[abr\]}
     {li  6 \[abr\]}}

   This is useful so that we can partition unions into maximal groups that can
   be safely reordered and left-factored.
   For example [("food" | "barbecue" | "foo" | "bar")] can safely be converted
   to [(("barbecue" | "bar") | ("food" | "foo"))] since the follower sets
   {[\[b\]]} and {[\[f\]]} are disjoint.
 *)

module type AnnotatableGrammar = sig
  type meta_t
  type annot_meta_t
  val source_pos : meta_t       -> SourcePosition.t
  val annotate   : meta_t       -> Unicode.Range.Set.t -> annot_meta_t
  val ranges     : annot_meta_t -> Unicode.Range.Set.t
end

(** Utilities for printing annotated grammars useful for testing. *)
module AnnotatedGrammarPrinter : functor (A : AnnotatableGrammar) -> sig
  val string_of_annotated_grammar : A.annot_meta_t Grammar.grammar -> string
end

module Followers (A : AnnotatableGrammar) : sig

  (**
    [followers g s] returns a grammar similar in structure to g but whose
    meta information contains follower information represented as range sets
    computed when starting from the given starts.
   *)
  val followers :
       A.meta_t Grammar.grammar
    -> A.meta_t Grammar.Start.t list
    -> A.annot_meta_t Grammar.grammar

end
