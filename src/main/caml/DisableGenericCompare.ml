(*
  Copyright 2014 Google, Inc.

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


(** Replaces the pervasive equality and comparison operators with ones that
    only operate on [int]s, so that we don't accidentally apply them to complex
    values. *)

(* Prevent accidental use of generic equality or comparison. *)
let (=)  (a:int) (b:int) = Pervasives.(=)  a b
let (<>) (a:int) (b:int) = Pervasives.(<>) a b
let (<)  (a:int) (b:int) = Pervasives.(<)  a b
let (<=) (a:int) (b:int) = Pervasives.(<=) a b
let (>)  (a:int) (b:int) = Pervasives.(>)  a b
let (>=) (a:int) (b:int) = Pervasives.(>=) a b
let (==) () () = failwith "disabled"
let (!=) () () = failwith "disabled"

let max (a:int) (b:int) = Pervasives.max a b
let min (a:int) (b:int) = Pervasives.min a b

let max_float (a:float) (b:float) = Pervasives.max a b
let min_float (a:float) (b:float) = Pervasives.min a b
(* TODO: Do these do the right thing around NaN and signed zeroes? *)

let str_eq (a : string) (b : string) = Pervasives.(=) a b
(** Substitute for [str1 = str2] *)

let chr_eq (a : char) (b : char) = Pervasives.(=) a b

let is_none x = Pervasives.(=) None x

let is_empty x = Pervasives.(=) [] x

let xor (a : bool) (b : bool) = Pervasives.(<>) a b

let xnor (a : bool) (b : bool) = Pervasives.(=) a b

let is_neg_or_neg_zero n = Pervasives.(<) (copysign 1.0 n) 0.0

let same     = Pervasives.(==)
let distinct = Pervasives.(!=)
(** Reference identity *)

let negate : 'a 'b . ('a -> 'b -> bool) -> 'a -> 'b -> bool =
  fun f a b -> not (f a b)
(** The negation of a binary predicate. *)

let compare (a : int) (b : int) = Pervasives.compare a b

let cmp_int = compare

let cmp_float (a : float) (b : float) = Pervasives.compare a b

let cmp_str (a : string) (b : string) = Pervasives.compare a b

let cmp_chr (a : char) (b : char) = Pervasives.compare a b

let cmp_bool (a : bool) (b : bool) = Pervasives.compare a b

module MakeSimpleCmp (X : sig type comparable end) : sig
  val compare : X.comparable -> X.comparable -> int
  (** A compare that's valid for simple symbolic values, or values that are
      guaranteed to be of different variants. *)

  val equal : X.comparable -> X.comparable -> bool
end = struct
  let compare (a : X.comparable) (b : X.comparable) = Pervasives.compare a b
  let equal   (a : X.comparable) (b : X.comparable) = Pervasives.(=) a b
end

(* Comparison operators for primitive types. *)
let (<=%) (a : char)  (b : char)  = Pervasives.(<=) a b
let (<%)  (a : char)  (b : char)  = Pervasives.(<)  a b
let (=%)  (a : char)  (b : char)  = Pervasives.(=)  a b
let (>=%) (a : char)  (b : char)  = Pervasives.(>=) a b
let (>%)  (a : char)  (b : char)  = Pervasives.(>)  a b
let (<%>) (a : char)  (b : char)  = Pervasives.(<>) a b

let (<=.) (a : float) (b : float) = Pervasives.(<=) a b
let (<.)  (a : float) (b : float) = Pervasives.(<)  a b
let (=.)  (a : float) (b : float) = Pervasives.(=)  a b
let (>=.) (a : float) (b : float) = Pervasives.(>=) a b
let (>.)  (a : float) (b : float) = Pervasives.(>)  a b
let (<.>) (a : float) (b : float) = Pervasives.(<>) a b
