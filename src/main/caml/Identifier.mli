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

(** A symbol in a grammar that serves variously as the name of a production,
    grammar variable name, grammar variable value name. *)

module Namespace : sig
  type t = private string

  val make : string -> t
  val to_string : t -> string

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val stringer : t Stringer.t

  val default : t
  (** The top-level namespace used for identifiers defined in a grammar
      which are not imported from other grammars and which do not
      reference builtin entities. *)

  val synthetic : t
  (** Namespace for identifiers introduced while desugaring syntactic sugar. *)

  val well_known : t
  (** Namespace for well-known variables that are in-scope without explicit
      declaration. *)

  val builtin_namespaces : t list
  (** Namespaces that cannot be added to be parsed user code. *)
end
(** Namespaces used to distinguish identifiers in imported grammars from
    ones in the importing grammars from synthetic identifiers used in
    desugaring. *)

type t = private Identifier of Namespace.t * string
(** An identifier in a grammar. *)

module Map : MapUtil.S with type key = t
module Set : SetUtil.S with type elt = t

val make : Namespace.t -> string -> t

val local_name : t -> string
val namespace : t -> Namespace.t

val compare : t -> t -> int
val hash : t -> int
val equal : t -> t -> bool

val stringer : t Stringer.t
val to_string : t -> string

val suffix : t -> string -> t
(** An identifier in the namespace but whose local name has the given suffix
    appended. *)
