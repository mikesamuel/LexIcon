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

(** Converts IL programs to Java code. *)

module Opts : sig
  module InputBufferType : sig
    type t =
      | CharSequence
      (** [java.lang.CharSequence] is an interface implemented by both
          [java.lang.String] and [java.lang.StringBuilder] so it is more
          flexible but requires virtual method dispatch per character lookup. *)
      | String
      (** [java.lang.String] allows static dispatch on character lookup. *)
    val stringer : t Stringer.t
    val compare : t Cmp.t
  end
  (** Specifies a java class that describes textual input whether. *)

  type t = {
    package           : JavaParseTree.JPackage.t;
    (** The package of the generated Java files *)
    input_buffer_type : InputBufferType.t;
    (** The type of the input buffer. *)
    token_class_name  : JavaParseTree.JIdent.t option;
    (** Name of a class which will be used to pool all regular expression
        matchers so that tools which call out to external tools to redo
        some of the work need not each have their own version of a regex
        matching method. *)
    comment_source    : bool;
    (** True to add inline comments showing which IL statement a Java
        statement originated from. *)
  }

  val default : t
  val stringer : t Stringer.t
  val compare : t Cmp.t
end

module Privates : sig
  type t = {
    prog_label   : Label.t;
    (** The label of the program from which the class was derived. *)
    signature    : Signature.t;
    (** The program signature. *)
    cuks         : CodeUnitKinds.t;
    (** The code-unit-kinds used by the tool. *)
    entry_method : JavaParseTree.jmember;
    (** The entry method. *)
    class_ref    : JavaParseTree.jclass_ref;
    (** A reference to the tool class. *)
    entry_ref    : JavaParseTree.jmethod_ref;
    (** A reference to the entry method that starts parsing. *)
    side_tables  : JavaParseTree.JExpr.t SideTable.FlavorMap.t;
    (** Expressions that evaluate to side-tables. *)
    mark_ref     : JavaParseTree.jfield_ref option;
    (** A reference to any mark indirection array. *)
    marks        : EvMarker.Set.t;
    (** A set of all marks used that can be used to decide whether
        post-processing stages can be skipped.
        For example, if no LR markers are used then we can skip
        LR pushback. *)
    pp_flags     : JavaParseTree.JExpr.t;
    (** A set of flags that the post-processor can use to avoid
        unnecessary work based on the marks that can appear on the
        output buffer. *)
  }
end
(** Describes the internals of a tool class. *)

module Interface : sig
  type t = {
    instance_type                     : JavaParseTree.JType.t;
    (** A super-class/interface for the instance object that reifies the tool
        allowing non-reflective first-class usage. *)

    tool_method_name                  : JavaParseTree.JIdent.t;
    (** The name of the method in the instance field's value that is overridden
        to invoke the tool. *)

    result_type                       : JavaParseTree.JRType.t;
    (** The return type for [public] methods and for the instance override
        methods.
        If this is [String] or [CharSequence] then there will also be
        [void] methods that takes an [Appendable] or [StringBuilder] and
        leave the result on that. *)

    needs_random_access_output_buffer : bool;
    (** True when post_process requires am output-buffer that can be inspected
        and mutated in place (i.e. a [StringBuilder] instead of an arbitrary
        [Appendable]). *)

    post_process_failure_modes        : JavaParseTree.JType.t list;
    (** The types of exceptions that can be thrown by the result of
        [post_process]. *)

    make_post_process_context         : (
      Privates.t -> (JavaParseTree.JExpr.t * JavaParseTree.JType.t) option
    );

    post_process                      : (
         privates       :Privates.t
      -> output_buffer  :JavaParseTree.JExpr.t * JavaParseTree.JType.t
      -> length_at_entry:JavaParseTree.JExpr.t option
      -> result_lhs     :JavaParseTree.JExpr.t option
      -> JavaParseTree.JStmt.t option
    );
    (** [post_process ~privates ~output_buffer ~length_at_entry result_lhs]
        is some statement that handle post-processing of parses results.
        [output_buffer] is an expression and type that describe the output
        buffer on which tokens and marks were accumulated.
        [length_at_entry] can be used to truncate a random-access output buffer
        to restore it to it's state before parsing started.
        [result_lhs] is some expression that can have the result assigned to it
        or [None] if the output should be left on an output buffer passed in.

        If the result is [None], then [translate] will not provide the public
        method that would have used the post-processing.
    *)

    fail                              : (
         privates       :Privates.t
      -> output_buffer  :JavaParseTree.JExpr.t * JavaParseTree.JType.t
      -> length_at_entry:JavaParseTree.JExpr.t option
      -> JavaParseTree.JStmt.t
    );
    (** [fail ~privates ~output_buffer ~length_at_entry] is a statement that
        ends execution and alerts the caller to the failure, typically by
        raising an exception. *)
  }
end
(** Specifies how the external interface for the tool is created.
    The external interface consists of several pieces.
    {ol
    {li A public interface consisting of several overridden
        [public static final] methods named after the tool kind (e.g. [encode])
        that call [run(...)] to parse the input, do any post-processing and
        produce a result or raise a failure-mode.}
    {li An extern interface so that other tools can call this tool via
        package-private methods.}
    {li A [public static final ToolAbstractClassType INSTANCE] field that
        reifies the tool so that it can be used as a first-class value without
        reflection, and so that it can appear as an entry in other tools
        side-tables.}
    }
*)

val translate :
     position_of_meta:('m -> SourcePosition.t)
  -> src_grammar:'m Grammar.grammar
  -> opts:Opts.t
  -> java_class_name:(Label.t -> Label.t)
  -> interface_for:(Label.t -> Interface.t)
  -> programs:'m CompiledPegs.t
  -> public_programs:Label.Set.t
  -> (Label.t option * Path.t * JavaParseTree.JFile.t) list
(** Translates the given programs to Java source files.
    @param position_of_meta
        The position in the source of a construct with the given meta used to
        populate comments and source map.
    @param src_grammar
        The grammar from which the program was generated.
    @param opts
        Options that control the generated code's interface.
    @param java_class_name
        The top-level class name within package that should encapsulate the
        program with the given label.
    @param interface
        [interface.post_process ...] produces code that is used to flesh out
        the public and extern methods that kick off parsing and compute the
        final result by doing post-processing on the output buffer with its
        accumulated tokens and marks.
    @param programs
        The compiled programs and associated side tables to translate.
    @param public_programs
        The labels of programs whose classes should be visible outside
        [package].
    @return [(program_label, base_path, java_parse_tree)] where [program_label]
        is [Some _] for each source file generated from an IL program, or [None]
        for an ancillary file like an [enum] generated from a grammar variable.
*)

val imports : (JavaParseTree.jpackage * JavaParseTree.JIdent.t) list
(** Useful imports for Java source files generated by this module. *)


val tool_instance_field_name : JavaParseTree.JIdent.t
(** The name of a [public static final] singleton field that references an
    object that encapsulates/reifies the tool to make the tool first-class
    without resorting to reflection. *)


val post_process_context_name : JavaParseTree.JIdent.t
(** Well-known name of a [public static final] field that stores side-tables
    and other context required by post-processing. *)
