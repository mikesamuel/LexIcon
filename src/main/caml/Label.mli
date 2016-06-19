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

type t = private string
(**
    A style-agnostic symbolic name in a programming language.
    A Label can be rendered using {i camelCase}, {i under_scored}, or
    any number of other ways, but a Label's identity does not depend on the
    details of its style or use of underscores.
 *)

type style =
  | LowerUnderscore  (** {v foo_bar v} *)
  | TitleUnderscore  (** {v Foo_Bar v} *)
  | UpperUnderscore  (** {v FOO_BAR v} *)
  | LowerCamelCase   (** {v fooBar v} *)
  | UpperCamelCase   (** {v FooBar v} *)

val is_label_str  : string -> bool

val make_stringer : ?style:style -> t Stringer.t
val stringer      : t Stringer.t
val of_string     : string -> t
val of_identifier : Identifier.t -> t
val to_string     : ?style:style -> t -> string
val prefix        : string -> t -> t
val suffix        : t -> string -> t

val compare       : t -> t -> int
val hash          : t -> int
val equal         : t -> t -> bool

module Map        : MapUtil.S with type key = t
module Set        : SetUtil.S with type elt = t
