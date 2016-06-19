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

(**
    A simple but inefficient implementation of {!PegParser} based on a stack
    interpreter.

    It supports left-recursion via
    {{:http://www.vpri.org/pdf/tr2008003_experimenting.pdf}Warth's}
    "growing the seed" scheme which
    {{:http://tratt.net/laurie/research/publications/html/tratt__direct_left_recursive_parsing_expression_grammars/}Tratt}
    showed is separable from Packrat.
*)


module type S = sig
  module Lang : PegOpInterp.LANG
  module Op   : PegOpInterp.OPERATOR with type 'm t    = 'm Lang.op
                                     and  type 'm lang = 'm Lang.t

  module Dot : sig
    val to_dot : out_channel -> 'm Lang.t -> unit
  end

  type 'm inp_sink
  (** An input sink that receive chunks of input, applies the parsing rules that
      are implied by states, and delegates interrupt handling to a user-supplied
      function. *)

  type 'm out_prod
  (** An output producer that has received all its input and which delegates
      side-effects and output computation to [Lang]. *)

  val start_path : 'm Lang.t -> ('m, 'm Op.t) PegRuntime.Path.t
  (** [start_path lang] is a path at the start machine of the given language. *)

  val make : ?logger :
    ('m, 'm Op.t) PegRuntime.logger
    -> (('m, 'm Op.t) PegRuntime.Path.t -> ('m, 'm Op.t) PegRuntime.Path.t)
    -> 'm Lang.t -> 'm inp_sink
  (** [make lang] is a parser that parses according to the given language. *)

  val parse_inputs : 'm inp_sink -> PegRuntime.input list -> 'm inp_sink
  (** [parse_inputs is inputs] is the path through inputs. *)

  val end_of_input : 'm inp_sink -> 'm out_prod
  (** [end_of_input is] is an interpreter that will operate on the inputs given
      knowing that no more inputs will be seen.

      Until the end of input is seen, some constructs including negative
      lookaheads like [(!char)]) cannot pass. *)

  val path_of : 'm inp_sink -> ('m, 'm Op.t) PegRuntime.Path.t
  (** [path_of is] is the path computed through the grammar that backs [is] for
      the inputs supplied to the input sink.
      Processing will pause if there is a character that could be appended to
      inputs to extend the match, or if an [\@Until] or [\@Embedded] section
      could find a match given more input past the end of the supplied inputs.
    *)

  val finish : 'm out_prod -> 'a Op.seed -> ('m, 'a) Op.context PegResult.t
  (** [finish parser op x] operates on [op's path] to try and find a match
      given the additional fact that previously supplied inputs form the whole
      input and applies all the operators seen on path that was successfully
      found.  Any result context is derived by applying [Op] functions to
      [Op.make_start_context lang x] where [lang] is the language passed to
      [make]. *)

  val parse :
       ?logger : ('m, 'm Op.t) PegRuntime.logger
    -> ?interrupt_handler : (
         ('m, 'm Op.t) PegRuntime.Path.t -> ('m, 'm Op.t) PegRuntime.Path.t)
    -> 'm Lang.t -> PegRuntime.input list
    -> 'a Op.seed -> (('m, 'a) Op.context) PegResult.t
  (** [parse lang inputs seed] is a convenience that parses the given inputs
      using the given language where [seed] is used to create the start context.
  *)

end
(** Parse operators that take textual input, parse it, and do computations and
    perform side-effects based on operators used during parse. *)

module Make :
       functor (Lang : PegOpInterp.LANG)
    -> functor (Op   : PegOpInterp.OPERATOR with type 'm t    = 'm Lang.op
                                            and  type 'm lang = 'm Lang.t)
    -> S with module Lang = Lang and module Op = Op
