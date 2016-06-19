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

(** Ways in which parts of plain old data can be encoded. *)

type t =
  | String
  (** Indicates that the grammar encodes a plain text value. *)

  | Char
  (**
    Inside [StringValue] indicates that the annotated grammar encodes a
    single character.
    Each annotation of this type must contain exactly one [CharValue]
    or [ScalarValue] annotation across each branch.
    The encoded character is determined using the following algorithm:
    {ul
    {- If there is a [CharValue] annotation with a string parameter,
      then that parameter specifies the encoded character.
      E.g. [\@CharValue\{"\\b"\} "\\\\n"].}
    {- If there is a [CharValue] annotation whose annotated
      grammar specifies a string of one character, that is the encoded
      character.  E.g. [\@CharValue \[^'\\n\\r\\\\\]].}
    {- If there is a [ScalarValue] annotation then the character
      encoded is the character whose integer value is encoded by the
      scalar value body interpreted as an integer in the base specified by
      the scalar value parameter if any or the inferred base (see Base)
      otherwise.}}
   *)

  | CharValue of Unicode.t option
  (**
    Inside a [Char] indicates that the character's value is
    specified by the annotated characters.
    The optional parameter indicates the encoded character.
   *)

  | ScalarValue of int option
  (**
    Inside a [Char] indicates that the character's value is
    specified by the annotated digits.
    The optional parameter indicates the base of the encoded digits:
    8 for octal, 10 for decimal, or 16 for hexadecimal.
   *)

  | KeyValueMap
  (**
    Indicates that the annotated text encodes a key value map.
    For example, JavaScript's ObjectLiteral production encodes an object with
    key/value pairs.

    This annotation works with other value type annotations as demonstrated
    below.
    {[
    JSONObject           := \@KeyValueMap "\{" JSONMemberList? "\}"          ;
    JSONMemberList       := JSONMember ("," JSONMemberList)?              ;
    JSONMember           := \@Key JSONString ":" \@Value JSONValue          ;
    JSONValue            := JSONNullLiteral
                          | JSONBooleanLiteral
                          | JSONObject
                          | JSONArray
                          | JSONString
                          | JSONNumber                                    ;
    JSONNullLiteral      := \@ValueNull "null"                             ;
    JSONBooleanLiteral   := \@ValueFalse "false" | \@ValueTrue "true"       ;
    JSONArray            := \@List ("\[" (JSONValue ("," JSONValue)* )? "\]");
    JSONString           := \["\] JSONStringCharacters? \["\]                 ;
    JSONNumber           := \@Number ...                                   ;
    JSONStringCharacters := JSONStringCharacter JSONStringCharacters?     ;
    JSONStringCharacter  := \@Char (JSONRawCharacter | JSONEscapeSequence) ;
    JSONRawCharacter     := \@CharValue\[^\x00-\x1f\x22\\\]
                          | JSONEscapeSequence                            ;
    JSONEscapeSequence   := "\\" \@CharValue \[/\x22\\\]
                          | \@CharValue\{"\x08"\} "\\b"
                          | \@CharValue\{"\x0c"\} "\\f"
                          | \@CharValue\{"\x0a"\} "\\n"
                          | \@CharValue\{"\x0d"\} "\\r"
                          | \@CharValue\{"\x09"\} "\\t"
                          | "\\u" \@ScalarValue (hex hex hex hex))         ;
    ]}
   *)

  | Key
  (**
    Inside a [KeyValueMap], specifies that the annotated text encodes a key.
   *)

  | Value
  (**
    Inside a [KeyValueMap], specifies that the annotated text encodes a
    value.
   *)

  | List
  (**
    Indicates that the annotated text encodes an ordered series of values.
   *)

  | Element
  (**
    Indicates that the annotated text encodes an element of a [List].
   *)

  | ValueFalse
  (**
    Indicates that the annotated text encodes the boolean value [false].
   *)

  | ValueTrue
  (**
    Indicates that the annotated text encodes the boolean value [true].
   *)

  | ValueNull
  (**
    Indicates that the annotated text encodes the special value [null].
   *)

  | Number
  (**
    Indicates that the annotated text encodes a number.
    Character sets that are sub-sets of the below have special meaning:
    {ul
     {li [\[+-\]] indicate a sign or exponent sign.}
     {li [\[0-7\]] indicate an octal digit.}
     {li [\[0-9\]] indicate a decimal digit or exponent digit.}
     {li [\[0-9A-Fa-f\]] indicate a hexadecimal digit.}
     {li [\[.\]] indicate a break between integer and fraction portion.}}
   *)

val equal : t -> t -> bool

val compare : t -> t -> int

val contained_by : t -> t -> bool
(** [contained_by inner outer] true if data of kind outer can contain data of
    kind inner. *)

val stringer : t Stringer.t


module Set : SetUtil.S with type elt = t
