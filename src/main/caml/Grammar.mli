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
  Datatypes for specifying annotated and scoped PEG grammars.

  The term "character" in this module does not refer to OCaml's [char] type
  but instead refers to a character as matched by grammatical productions which
  may be a char (octet), UTF-16 code-unit, or Unicode code-point/scalar value.
 *)

module Ordering : sig
  type t =
    | Ordered   (** *)
    | Unordered (** *)
  (** Indicates whether child order is significant in a Union. *)

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val stringer : t Stringer.t
end

module Recursivity : sig
  type t =
    | Flat
    | Recursive
  val compare : t Cmp.t
  val equal : t -> t -> bool
  val stringer : t Stringer.t
end
(** Marks a variable as usable in a recursive invocation -- it can be masked
    by another variable of the same name. *)

(** A specification for a set of strings. *)
type 'meta grammar_body =

  | Annotation of 'meta * 'meta annotation * 'meta grammar_body
  (**
    A hint that conveys information to analytic tools or that affects the
    semantics of the grammar by introducing or manipulating scopes.
   *)

  | CharSet of 'meta * Unicode.Range.Set.t
  (**
    Matches a single character in the given range set.
    A "character" in this context does not necessarily correspond to an
    OCaml [char].  Some grammars are defined in terms of Unicode code-points or
    scalar values, and some in terms of UTF-16 code units, and others in terms
    of octets.
   *)

  | Concatenation of 'meta * 'meta grammar_body list
  (**
    Matches strings formed by concatenating any string specified by the
    first child with any string specified by the second, etc.
    The childless concatenation matches the empty string.
   *)

  | Difference of 'meta * 'meta grammar_body * 'meta grammar_body
  (**
    [Difference (_, minuend, subtrahend)] matches the set of individual
    characters matched by minuend that are not also matched by subtrahend.
    It is a runtime error if both children do not eventually resolve to
    CharSets.
   *)

  | Reference of 'meta * Identifier.t
  (**
    [Reference (_, name)] is a non-terminal reference to body in
    [Production (_, name, body)].
   *)

  | Repetition of 'meta *'meta grammar_body
  (**
    [Repetition (_, body)] matches the union of those matched by body,
    \{[(Concatenation (), [body, body])]\},
    \{[(Concatenation (), [body, body, body])]\}, ...
   *)

  | Union of 'meta * Ordering.t * 'meta grammar_body list
  (**
    Specifies the union of the set of strings specified by its children.
    Order is significant since the grammar is a PEG grammar but sometimes we
    may prove that a particular union contains only elements that are mutually
    exclusive, so the second field may indicate that the union is unordered.
   *)

  | Panic of 'meta
  (** Aborts all further processing without either succeeding or failing. *)

and 'meta annotation =
  | Data of POD.t
  (** Describes how the annotated portion maps encodes a data value. *)

  | Denormalized of 'meta grammar_body option * Var.Pred.t
  (**
    Indicates that the annotated portion encodes a value in a way that
    is non-standard, but can be re-encoded in a way that is less problematic.

    When the body is present, it must specify a string that is substituted for
    the original.

    When absent, the body must encode a value which is re-encoded using prior
    options in the path to the annotation.
    Source code comments often contain details programmers did not intend to
    broadcast.  Those can be elided by marking comments or comment bodies with
    this annotation with the empty string as the replacement value.

    [\@Denormalized{ when goal san}] indicates that the content should be
    renormalized when the well-known variable [goal] has the value [san],
    which is set for sanitizer tasks, but not decoding.  Variables are sampled
    at the time the annotation body is exited.

    [\@Implied{x}] is syntactic sugar for [\@Denormalized{x}()] and indicates
    that the parameter text will not be seen on input, but
    should be included on output before the annotated node is entered.
    The annotated node must be the empty string.
   *)

  | Embedded of 'meta grammar_body * Var.Pred.t
  (**
    [Embedded (inner, when, success_var)] can be attached to a [String]
    to indicate that the annotated node's content is an encoded form of the
    specified production when the [when] production evaluates to true after
    processing the body.

    Parsing of [\@Embedded] involves some desugaring.
    [\@Embedded \{b, Success: p\}] desugars to
    [\@Embedded \{\@Set\{Success, pass\} b | \@Set\{Success, fail\} \[^\]*\}]
    and [\@Embedded \{b : p\}] desugars to [\@Embedded \{b | PANIC : p\}].

    If [success_var <> None], then that variable is {!annotation.Set} to
    [pass] if inner matches the embedded string or [fail] if it does not.

    If [success_var = None], then failure of inner to match the embedded string
    means that application of the grammar must stop without either matching or
    not.
  *)

  | CaseFold of CaseFold.t
  (**
    Indicates that the content of the annotated node is case-folded with
    the specified transform.
   *)

  | Scope of Var.Name.t * Recursivity.t
  (** [Scope (name, recursivity)] introduces a variable with the given name
      into scope.  If the {!Recursivity} is {!Recursivity.t.Recursive} then the
      variable may be masked by another variable with the same name. *)

  | Set of Var.Name.t * Var.Expr.t
  (**
     Reachable from a [Scope], sets the value of the named scope variable to
     the result of reducing expr when the annotated portion matches.
     Setting occurs before control reaches the annotation body.
  *)

  | If of Var.Pred.t
  (**
    The annotated node succeeds only when the given predicate is true prior
    to entering the node.
   *)

  | Until of 'meta grammar_body
  (**
    Indicates that the annotated production matches until the first match of
    the parameter.
   *)

  | Override of Identifier.t * 'meta grammar_body
  (**
    Allows overriding of a non-terminal within a production (and downstream
    from it).
    For example, [\@override\{char, ascii\}] indicates that in the annotated
    node and any contained nodes, [char] means [ascii] instead of the default
    [unicode] which affects inverted character sets ([\[^...\]]) which are
    syntactic sugar for [char-\[...\]].
   *)

  | Entrust of Identifier.t * Var.Names.t * Var.Pred.t
  (**
     [Entrust (extern_name, read, pred)] performs the body, but if [pred]
     evaluates to true after the body, then a tool based on the grammar may
     call-out to an external function via the name [extern_name] which may
     {ul
       {li inspect any input read by the body,}
       {li inspect / modify any output generated by the body,}
       {li inspect the values of the variables in [read],}
       {li override the values of any variables that could have been set by the
           body (statically, any variables V s.t. there is an [\@Set{X, ...}]
           reachable from the body without an intervening [\@Scope{X, ...}])}
     }
   *)
(** An extra-grammatical notation that may affect
    {ul
      {li how the grammar is interpreted, e.g. [\@If]}
      {li specify the relation of substrings to data values, e.g. [\@Data]}
      {li specify a relation between substrings in the language
          and semantically equivalent and more noramal or safer substrings,
          e.g. [\@Denormalized]}
      {li provide syntactic sugar, e.g. [\@CaseFold]}
    } *)


type 'meta production = Production of 'meta * Identifier.t * 'meta grammar_body
(** A named grammar node that may be referenced by its name. *)

type 'meta headers = {
  namespace:         Identifier.Namespace.t;
  (** The namespace of the grammar, which is often used to lookup the start
      production. *)
  grammar_variables: 'meta Var.Decls.t;
  (** The set of values that each variable can take.
      Simplification may result in some values not being used, in which case
      they may be replaced with [None] though they are not removed from the list
      because the index of the variable in the list may be significant for
      backwards compatibility with back-ends that represent values as small
      integers. *)
}
(** Information about the grammar including constraints. *)

type 'meta grammar = Grammar of 'meta * 'meta headers * 'meta production list
(** A top level grouping of productions *)

type 'meta node =
  | A of 'meta annotation
  | G of 'meta grammar
  | P of 'meta production
  | N of 'meta grammar_body

exception Node_type_mismatch of string
(** Raised when a node of one type is seen where nodes of another type are
  expected. *)

val prod_with_name : 'meta grammar -> Identifier.t -> 'meta production

val prod_with_name_opt :
  'meta grammar -> Identifier.t -> 'meta production option

val body_with_name : 'meta grammar -> Identifier.t -> 'meta grammar_body

val body_with_name_opt :
  'meta grammar -> Identifier.t -> 'meta grammar_body option

val body_meta : 'meta grammar_body -> 'meta

val grammar_meta : 'meta grammar -> 'meta

val prod_meta : 'meta production -> 'meta

val meta : 'meta node -> 'meta
(** The metadata associated with the given node. *)

val body_map_meta : ('meta node -> 'meta -> 'ometa)
  -> 'meta grammar_body -> 'ometa grammar_body
(** [body_map_meta f n] is a body equivalent to n but with meta-data
  [f n (body_meta n)] and all children recursively transformed depth-first. *)

val annot_map_meta : ('meta node -> 'meta -> 'ometa)
  -> 'meta annotation -> 'ometa annotation

val prod_map_meta
  : ('meta node -> 'meta -> 'ometa) -> 'meta production -> 'ometa production

val grammar_map_meta
  : ('meta node -> 'meta -> 'ometa) -> 'meta grammar -> 'ometa grammar

val headers_map_meta
  : ('meta -> 'ometa) -> 'meta headers -> 'ometa headers

val map_meta : ('meta node -> 'meta -> 'ometa) -> 'meta node -> 'ometa node

val body_map_children
  : ('meta node -> 'meta node) -> 'meta grammar_body -> 'meta grammar_body

val annot_map_children
  : ('meta node -> 'meta node) -> 'meta annotation -> 'meta annotation

val map_children : ('meta node -> 'meta node) -> 'meta node -> 'meta node

val map_deep :
     ?pre:( 'meta node -> 'meta node)
  -> ?post:('meta node -> 'meta node)
  -> 'meta node -> 'meta node
(** [map_deep ~pre ~post n] maps every node under [n], applying [pre] in a
    pre-order traversal followed by [post] in a post-order traversal. *)

val fold : ('a -> 'meta node -> 'a) -> 'a -> 'meta node -> 'a
(**
  [fold f v0 n] applies f in turn to each wrapped child of [n] thus
  [f ... (f (f v0 child0) child1) ... child-n]
 *)

exception Do_not_descend
(** Can be raised by a folder function to short-circuit descent. *)

val fold_body_deep :
  ?descend_into_annotations:bool
  -> ('a -> 'meta node -> 'a) -> 'a -> 'meta node -> 'a
(** [fold_body_deep f v0 n] folds f over the descendants of [n] depth-first
    but does not descend into annotation parameters unless
    [descend_into_annotations] or explicitly passed an {!node.A} node.

    If [f] raises [Do_not_descend] then folding will resume but without
    recursing to the descendants of that node.
*)

module Compare : sig
  val body : 'm grammar_body -> 'n grammar_body -> int
  val annotation : 'm annotation -> 'n annotation -> int
  val production : 'm production -> 'n production -> int
  val headers : 'm headers -> 'n headers -> int
  val grammar : 'm grammar -> 'n grammar -> int
end

val compare : 'm node -> 'n node -> int

module Equal : sig
  val body : 'm grammar_body -> 'n grammar_body -> bool
  val annotation : 'm annotation -> 'n annotation -> bool
  val production : 'm production -> 'n production -> bool
  val headers : 'm headers -> 'n headers -> bool
  val grammar : 'm grammar -> 'n grammar -> bool
end

val equal : 'm node -> 'n node -> bool
(**
  Identical to [(map_meta (fun _ -> ()) a) = (map_meta (fun _ -> ()) b)] but
  more efficient.
 *)

val children : 'meta node -> 'meta node list
(** The children of the given nodes. *)

module Start : sig
  type 'm t = private
    | Named of Identifier.t     (* Start with the named production *)
    | Body  of 'm grammar_body  (* Start at the given grammar chunk *)

  val to_body : 'm grammar -> 'm t -> 'm grammar_body
  (** [to_body g start] is the body for start in [g]. *)

  val of_body : 'm grammar_body -> 'm t
  (** [of_body b] is a start such that [of_body (to_body b)] is equivalent
      to [b] in any grammar barring unresolved references. *)

  val contextualize : 'm grammar -> 'm t -> 'm t
  (** [contextualize g x] is equivalent to [x] in [g], but when [x] is an
      {!of_body} reference that matches a production in [g] it is the name of
      that production. *)

  val named   : Identifier.t -> 'm t
  (** [named id] is a start that is equivalent to
      [of_body (Reference (_, id))]. *)

  val name    : 'm t -> Identifier.t option
  (** [name s] is [Some s] if [s] is a named start. *)

  val referenced_by : 'm t list -> Identifier.Set.t
  (** The set of identifiers referenced from within the given starts. *)

  val map_meta : ('m -> 'n) -> 'm t -> 'n t
  (** Remaps meta_data. *)

  val equal : 'a t -> 'b t -> bool

  val compare : 'a t -> 'b t -> int
end
(** A start point for grammar operations which is interpreted in the context of
    a grammar. *)

module type Reporting = sig
  type meta_t

  val source_pos : meta_t -> SourcePosition.t
  (** Extracts a source position from node meta info.
      Useful for error reporting *)

  val join : meta_t list -> meta_t
  (** Produces the meta info for a parse tree node that is created
      from several. *)
end
(** Allows extracting of source positions for error messages. *)

module SimpleReporting : Reporting with type meta_t = SourcePosition.t

module type Productions = sig

  val define : unit production -> unit
  (** Called when a production is defined by the DSL below. *)

  val ns : Identifier.Namespace.t
  (** A default namespace for identifiers associated with these productions. *)

end
(** A group of productions such as those built by a GrammarBuilder *)

module GrammarBuilder : functor (P: Productions) -> sig

  val ( ~? ) : unit grammar_body -> unit grammar_body
  (** [~? x] optionally matches x.  It is syntactic sugar for [(x |: ())]. *)

  val ( ~+ ) : unit grammar_body -> unit grammar_body
  (** [~+ x] matches 1 or more occurrences of x. *)

  val ( ~* ) : unit grammar_body -> unit grammar_body
  (**
    [~* x] matches 0 or more occurrences of x.
    It is syntactic sugar for [~?(~+x)] or [(~+x |: ())].
   *)

  val ( --- ) : int -> int -> unit grammar_body
  (** [u --- v] is a character set including codepoints in the given range. *)

  val ( -- ) : char -> char -> unit grammar_body
  (** [('A'--'Z')] is a [CharSet] including characters in the given range. *)

  val ( - ) : unit grammar_body -> unit grammar_body -> unit grammar_body
  (** [a - b] is shorthand for [Difference ((), a b)]. *)

  val ( ~^ ) : unit grammar_body -> unit grammar_body
  (**
    [~^x] is shorthand for [char - x] so if [char] is defined as the
    character set containing all Unicode code-points, then [~^x] is any
    code-point except those specified by [x].
   *)

  val ( @ ) : unit grammar_body -> unit grammar_body -> unit grammar_body
  (** [a @ b] is shorthand for the concatenation of a and b. *)

  val ( |: ) : unit grammar_body -> unit grammar_body -> unit grammar_body
  (** [a |: b] is shorthand for the union of a and b. *)

  val ( ! ) : string -> unit grammar_body
  (** [!"foo"] is syntactic sugar for [('f'--'f') @ ('o'--'o') @ ('o'--'o')] *)

  val r : string -> unit grammar_body
  (** [r "foo"] is a reference to the non-terminal named "foo" *)

  val ( := ) : unit grammar_body -> unit grammar_body -> unit
  (**
    [p := x] defines (via [P.define]) a production with a name from p and
    body x.
   *)

end
(**
  [module G1 = Grammar()] can be opened to allow constructing of a grammar
  using a BNF like syntax via abuse of operator overloading.
 *)
