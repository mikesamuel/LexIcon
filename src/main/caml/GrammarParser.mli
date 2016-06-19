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
  Defines a grammar for annotated PEG grammars and parse and serialize
  operations.


  {3 Ignorable Tokens}

  ASCII white-space characters are ignored as are C++ style
  [// line comments] and [/* block comments */].


  {3 Grammars}

  Grammars consist of
  {ol
    {li an optional header containing file-wide declarations}
    {li a body of zero or more productions which define syntactic structures}
    {li a footer with zero or more imports directives that allow referencing
        of other grammars.}}


  {3 Header}

  A grammar starts with an optional header of the form,

  {[
  \{
    keyword = value ;
    Var_name <: (value0, value1, ...);
    ...
  \}]}

  where the [namespace] keyword establishes the {!Identifier.Namespace.t} for
  raw identifiers in the body of the grammar, and the [<:] clauses
  are {!Var.Pred}s that list the grammar variables and their associated
  values.  The order of values is significant since some backends define
  [enum] types which map values to integers, and the positional order of
  declaration here is preserved in such transformations.

  If the value list is followed by a [*] then the variable is treated as
  a multi-valued variable.  Backends can convert multi-valued variables to
  bit-fields where a bit index corresponds to the position in the symbol
  definition list.

  A grammar variable declaration may also use the domain of a previously
  declared grammar variable

  {[
    Var0     <: (a, b, c);
    Var1     <: Var0;  // Same domain
    Multivar <: Var0*; // Same domain symbols but multi-valued.
  ]}


  {3 Productions}

  The header is followed by zero or more [Production]s of the form

  [name := body ;]

  where name is an [Identifier] as described below, and
  body is a [grammar_body] as described below.

  The productions are followed by zero or more imports
  also described below.


  {3 NonTerminals and Identifiers}

  {!Identifier}s are used as [NonTerminal]s (the name of a production or a
  reference to the right-hand side of a production), as an annotation name,
  or as the name or value of a scoped variable.


  {3 Strings and CharSets}

  A string is a series of characters inside double quotes or single quotes:
  ["Hello, World!"].
  Strings are represented as [Concatenation]s of [CharSet]s where
  each char-set matches a sole code-point.
  [CharSet] syntax mirrors that of Perl5RE.
  {ul
   {li [\[H\]] - The letter 'H'.  Identical to ["H"]}
   {li [\[a-z\]] - The set of upper-case ASCII letters}
   {li [\[A-Za-z\]] - The set of ASCII letters}
   {li [\[\\\[\\\]\]] - Square brackets.}
   {li [\[\\n\\r\]] - ASCII line break characters.}
   {li [\[\\x0A\\x0D\]] - The same ASCII line break characters.}
   {li [\[\\uABCD\]] - A single code-point from the basic multilingual plane.}
   {li [\[\\U0001D1E4\]] - A single supplemental code-point.}
   {li [\[\[:Lu:\]\]] - The Unicode upper-case letter group.}
   {li [\[^a-z\]] - Syntactic sugar for [char-\[a-z\]].}}

  Inverted character sets, [\[^...\]] are syntactic sugar for [char-\[...\]].
  The [char] production is defined in DefaultProductions but may be overridden.

  Character sets are affected by the case-folding annotations.


  {3 Subtraction}

  [\[A-Z\]-\[SUB\]] is the character set that contains all and only
  characters in the left that do not also appear in the right.
  [\[A-Z\]-\[SUB\]] is the same as [\[AC-RTV-Z\]].

  The [-] operator may be used to connect arbitrary body parts as
  in [unicode-(low_surrogate | high_surrogate)] but the parts must
  simplify to character sets.


  {3 Grouping}

  Parentheses are used for grouping and only for grouping.
  Operator precedence is per the below.
  {ul
   {li Char-set difference : [a-b] - Highest}
   {li Repetition suffix : [a+], [a*], [a?] - High}
   {li Annotation : [\@a b] - Medium}
   {li Concatenation : [a b] - Low}
   {li Union : [a | b] - Lowest}
  }

  All binary operators are left-associative.


  {3 Repetition}

  [x+] is the non-terminal [x] repeated 1 or more times.

  [x*] is the same repetition but possibly zero times.
  It is equivalent to [(x+ | )].

  [x?] is zero or one occurrences of [x].  It is equivalent to [(x | )].


  {3 Concatenation}
  Adjacent body parts are concatenated.  ["Hello"\[,\]" World!"]
  is equivalent to ["Hello, World!"].

  Parts to be concatenated may be separated by spaces, so the
  pair of non-terminals, [Hello World], is the same as
  [(Hello)(World)].


  {3 Union}

  Multiple different possibilities may be expressed as a union indicated
  by the binary [|] operator.

  [a | b | c] is the language that is the union of the languages
  defined by the non-terminals [a], [b], and [c].

  Since union is lower precedence than concatenation,
  [a | b c | d] is equivalent to [a | (b c) | d].

  Order is significant in unions since this is a PEG grammar.


  {3 Annotation}

  Annotations start with an "\@" symbol followed by an identifier.
  They may optionally have comma-separated parameters surrounded by curly
  brackets.  The body of an annotation follows it.
  [\@Name \{param1,param2,45\} body].

  Annotation parameters can be arbitrary body parts, or decimal integers.
  Annotation is lower precedence than repetition and difference but higher
  precedence than concatenation.

  Some annotations can contain a {!Var.Pred} predicate that specifies when
  the annotation takes effect.  If present, it appears after the last parameter
  following a colon ([:]).


  {3 Empty strings}

  The empty concatenation can be represented by [()] or by [""].


  {3 Impossible matches}

  A character set with no character ranges, [\[\]], is the language
  containing no strings.  It will be simplified out of unions --
  [(a | \[\])] is equivalent to [a].


  {3 Negative lookahead}

  [!(x)] is a zero-width negative lookahead.
  It is actually syntactic sugar for

  {[
  \@Scope\{NEG_LA\} (
    (x \@Set\{NEG_LA,FAIL\} () | \@Set\{NEG_LA,PASS\} ())
    \@If\{NEG_LA = PASS\} ()
  )]}


  {3 Imports}

  An import directive can also appear at the top-level after productions.

  [\@import \{ "path-to-grammar.g" \} \{
      start     := start ;
      sub_local := sub_imported ;
    \};]

  says to recursively load the grammar at ["path-to-grammar.g"]
  (the parse functions take a function that resolves paths) and
  namespaces all the imported productions so that their names do not
  conflict with any previously loaded productions (or productions
  that might be added as via {!DefaultProductions.augment}) and
  create an alias [start] for the production originally named
  [start] in the loaded grammar, and create an alias [sub_local]
  for the production originally named [sub_imported].

  For example, the grammar

  [start := "foo" bar; \@import \{"bar.g"\} { bar := start }]

  where ["bar.g"] contains

  [start := "bar" baz; baz := "baz"]

  is equivalent (modulo arbitrary choice of namespaces) to

  [start := "foo" bar; bar := bar.start;
   bar.start := "bar" bar.baz; bar.baz := "baz"]

  In the alias section, a single identifier [prod_name] is syntactic sugar for
  [prod_name := prod_name].

  The parser chooses a namespace that does not conflict with
  of any previously loaded grammars.  If the imported grammar does not specify
  a namespace in its header then the parser creates one by including enough of
  the pathname of the loaded grammar path to ease debugging.
  It may include a number to disambiguate namespaces.  Given a set of
  production names and load paths, namespaces are chosen deterministically.

 *)


(**
  Precedence of operators parsed by the grammar parser
  ordered loosest to tightest.
 *)
type precedence =
    GrammarPrec
  | ProdPrec
  | UnionPrec
  | ConcatenationPrec
  | AnnotationPrec
  | PostfixPrec
  | DifferencePrec
  | AtomPrec


val make_body_stringer :
     ?str_meta:'meta Stringer.t
  -> ?prec:precedence -> 'meta Grammar.grammar_body Stringer.t

val body_stringer : 'meta Grammar.grammar_body Stringer.t

val make_annot_stringer :
  ?str_meta:'meta Stringer.t -> 'meta Grammar.annotation Stringer.t

val annot_stringer : 'meta Grammar.annotation Stringer.t

val make_prod_stringer :
  ?str_meta:'meta Stringer.t -> 'meta Grammar.production Stringer.t

val prod_stringer : 'meta Grammar.production Stringer.t

val make_headers_stringer :
  ?str_meta:'meta Stringer.t -> 'meta Grammar.headers Stringer.t

val headers_stringer : 'meta Grammar.headers Stringer.t

val make_grammar_stringer :
  ?str_meta:'meta Stringer.t -> ?include_headers:bool
  -> 'meta Grammar.grammar Stringer.t

val grammar_stringer : 'meta Grammar.grammar Stringer.t

val make_start_stringer :
  ?str_meta:'meta Stringer.t -> 'meta Grammar.Start.t Stringer.t

val start_stringer : 'meta Grammar.Start.t Stringer.t

val make_stringer : ?str_meta:'meta Stringer.t
  -> 'meta Grammar.node Stringer.t

val stringer : 'meta Grammar.node Stringer.t

type parser_callback =
     Path.t -> ByteInput.t -> SourcePosition.t
  -> SourcePosition.t Grammar.grammar
(** A callback that receives a canonicalized path, an input stream, and a debug
    position for the start of the stream, and returns the parsed result of that
    input. *)

type grammar_loader =
  parser_callback -> Path.t -> SourcePosition.t Grammar.grammar

val default_grammar_loader : grammar_loader
(** A grammar loader that just raises [Failure.Cannot_load].
    @raise Failure.Cannot_load whenever called. *)

val parse_grammar :
     ?grammar_loader:grammar_loader
  -> ByteInput.t -> SourcePosition.t -> SourcePosition.t Grammar.grammar
(** [parse ~grammar_loader:gl inp pos] parses a grammar from [inp] where the
    first byte read from [inp] is treated as being at [pos], [gl] is used to
    resolve paths in [\@import ...] annotations.

    The parsed grammar is in the {!Identifier.Namespace.default} unless the
    header block does contains an explicit namespace hint.

    The default [~load_grammar] just raises [Failure.Cannot_load].

    @raise Failures.Bad_syntax when the input contains syntactic problems.
    @raise Utf8.Bad_octet when the input is not valid UTF-8.
    @raise Failures.Cannot_load when an import cannot be loaded by
      [~load_grammar] or when [\@import ...] statements cause very deep
      importing which might indicate cyclic imports. *)

val parse_grammar_body :
     ByteInput.t -> SourcePosition.t -> SourcePosition.t Grammar.grammar_body
(** [parse_grammar_body inp pos] parses the right hand side of a production.

    @raise Failures.Bad_syntax when the input contains syntactic problems.
    @raise Utf8.Bad_octet when the input is not valid UTF-8. *)

val resugar_negative_lookahead :
  'a Grammar.grammar_body -> 'a Grammar.grammar_body option
(** [resugar_negative_lookahead b] is [Some x] when [b] is [!(x)] or [None]
    otherwise. *)

val desugar_negative_lookahead :
     Var.Name.t -> (unit -> 'a) -> (unit -> 'a)
  -> 'a Grammar.grammar_body -> 'a Grammar.grammar_body
(** [desugar_negative_lookahead var_name lookahead_meta end_meta b] is the
    equivalent of the grammar [!(b)].

    @param var_name is the name of a variable not used in [b]
    @param lookahead_meta produces meta-data for the lookahead node.
    @param end_meta produces meta-data for a node immediately after [b] that
           consumes no characters.
*)
