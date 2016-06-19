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

(** Common operators for inferring information about grammars. *)

val conservative_and_predicate :
     ?on_missing_referent:(Identifier.t -> bool)
  -> (unit Grammar.node -> bool option)
  -> 'a Grammar.grammar
  -> 'a Grammar.node
  -> bool
(** [conservative_and_predicate p g n] is true if and only if [p node] is not
    [Some false] for all nodes reachable from [n] where references are
    resolved in [g].
    Raises [Not_found] if there is a reference reachable from [n] that is
    unresolved in [g].
    More efficient if curried with [p] and [g] once before calling with
    a variety of [n]. *)

val stackwise_reaches :
     ('m Grammar.node -> 'a)
  -> ('m Grammar.grammar_body list -> 'a -> 'a -> 'a)
  -> ('m Grammar.grammar_body list -> 'a -> 'a -> 'a)
  -> 'a
  -> 'm Grammar.grammar
  -> 'm Grammar.grammar_body
  -> ('m * 'a) Grammar.grammar * ('m * 'a) Grammar.grammar_body
(** [reaches initial reached derive x g start] computes reachability from
    [start] through [g].
    It first annotates the tree by calling [initial node] to create an ['a]
    for each node.
    Then it walks all possibly (acyclic) paths from [start] through [g] storing
    nodes on a list with most recently entered at the head.
    At each node in the walk, it calls
    [reached node_stack prior_value parent_value] to update the reached value
    for that node, and calls [derive node_stack current_value parent_value] to
    find the value to propagate to children.
    Finally, it returns [g] and [start] annotated with the values that reached
    them. *)
