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


(* A pre-sized bucket of bits *)

include DisableGenericCompare

type t = (int * bytes)

exception Out_of_bounds of int

let make n = (n, Bytes.make ((n + 7) lsr 3) '\x00')

let length (n, _) = n

let get (_, bb) i =
  let byte = int_of_char (Bytes.get bb (i lsr 3)) in
  1 = ((byte lsr (i land 0B111)) land 1)

let set ?(value=true) (n, bb) i =
  if i >= n then
    raise (Out_of_bounds i)
  else begin
    let byte = int_of_char (Bytes.get bb (i lsr 3)) in
    let nbyte =
      if value then
        byte lor (1 lsl (i land 0B111))
      else
        byte land (lnot (1 lsl (i land 0B111))) in
    Bytes.set bb (i lsr 3) (char_of_int nbyte)
  end

let fold ?(value=true) f (n, bb) x0 =
  let n_bytes = Bytes.length bb in
  let rec fold_tail i byte x =
    if byte = 0 then
      if i = 0 then x
      else
        let next_byte = int_of_char (Bytes.get bb (i-1)) in
        fold_tail (i-1)
          (if value then
              next_byte
           else
              next_byte lxor lnot (~-1 lsl (min 8 (n - ((i-1) lsl 3)))))
          x
    else
    if 0 = byte land             0B11110000 then
      if 0 = byte land           0B00001100 then
        if 0 = byte land         0B00000010 then
          fold_tail i (          0B00000000) (f ((i lsl 3)      ) x)
        else
          fold_tail i (byte land 0B00000001) (f ((i lsl 3) lor 1) x)
      else
        if 0 = byte land         0B00001000 then
          fold_tail i (byte land 0B00000011) (f ((i lsl 3) lor 2) x)
        else
          fold_tail i (byte land 0B00000111) (f ((i lsl 3) lor 3) x)
    else
      if 0 = byte land           0B11000000 then
        if 0 = byte land         0B00100000 then
          fold_tail i (byte land 0B00001111) (f ((i lsl 3) lor 4) x)
        else
          fold_tail i (byte land 0B00011111) (f ((i lsl 3) lor 5) x)
      else
        if 0 = byte land         0B10000000 then
          fold_tail i (byte land 0B00111111) (f ((i lsl 3) lor 6) x)
        else
          fold_tail i (byte land 0B01111111) (f ((i lsl 3) lor 7) x) in
  fold_tail n_bytes 0 x0

let map ?(value=true) f bb =
  fold ~value:value (fun i tail -> (f i)::tail) bb []
