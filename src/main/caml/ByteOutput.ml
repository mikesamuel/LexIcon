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

(** Defines a byte output stream type. *)

include DisableGenericCompare

(**
  Given a [b : t], the call [b buffer left right] writes
  [String.sub buffer left (right - left)] to the underlying channel.
 *)
type t = string -> int -> int -> unit

let of_out_channel oc : t = fun buf lt rt -> output_substring oc buf lt (rt - lt)

let of_bytes ?(off=0) ?(limit=None) b : t * (unit -> int * int) =
  let byt = ref b in
  let limit = match limit with | Some n -> n | _ -> Bytes.length b in
  let pos = ref off in
  (fun buf lt rt ->
    let cursor = !pos in
    let to_write = rt - lt in
    if to_write > limit - cursor then
      raise (Failure "buffer too small");
    if to_write <> 0 then begin
      Bytes.blit_string buf lt !byt cursor to_write;
      pos := cursor + to_write;
      if cursor + to_write = limit then
        (* Release underlying string for GC. *)
        byt := Bytes.empty
    end),
  (fun () -> off, !pos)

type buffer = {
  mutable chunks :  (Bytes.t * int ref) list;
  mutable len : int
}

module Buffer = struct

  type t = buffer

  let make ?(size=128) () = {
    chunks = [(Bytes.create (max size 16), ref 0)];
    len = 0
  }

  let length b = b.len

  let sub_internal b lt rt len =
    let chunks = b.chunks in
    let s = Bytes.create (rt - lt) in
    let rec copy art chunks = match chunks with
      | [] ->
        if art <> 0 then failwith "size changed during copy";
        ()
      | (chunk, pos_ref)::prior_chunks ->
        let pos = !pos_ref in
        let alt = art - pos in
        (* [alt, art) is now the absolute range of chars in the buffer as a
           whole that correspond to chunk. *)
        if art > lt then begin
          (* Clip [alt, art) to the region we want to copy. *)
          let clipped_alt = max lt alt in
          let clipped_art = min rt art in
          let n_to_copy = clipped_art - clipped_alt in
          if n_to_copy > 0 then begin
            Bytes.blit chunk (clipped_alt - alt) s (clipped_alt - lt)
              n_to_copy
          end;
          copy alt prior_chunks
        end in
    copy len chunks;
    Bytes.to_string s

  let sub b lt rt = sub_internal b lt rt (length b)

  let char_at b i =
    (* TODO: rewrite to be efficient *)
    (sub b i (i+1)).[0]

  let to_string b =
    let len = length b in
    sub_internal b 0 len len

  let truncate b rt =
    if rt = 0 then begin
      b.len <- 0;
      let (_, size_ref) as chunk = List.hd b.chunks in
      b.chunks <- [chunk];
      size_ref := 0;
    end else
      let len = length b in
      if 0 <= rt && rt <= len then begin
        let rec discard chunks n_to_discard =
          if n_to_discard = 0 then
            chunks
          else match chunks with
            | [] -> failwith ""
            | (_, size_ref)::rest ->
              let size = !size_ref in
              if size <= n_to_discard then
                discard rest (n_to_discard - size)
              else begin
                size_ref := size - n_to_discard;
                chunks
              end in
        b.chunks <- discard b.chunks (len - rt);
        b.len <- rt
      end else
        invalid_arg "length out of range"

  let append_chars_from blit buffer buf lt rt =
    let rec append_from lt =
      let n_to_write = rt - lt in
      if n_to_write < 0 then invalid_arg "buffer.append";
      let (chunk, pos_ref) = List.hd buffer.chunks in
      let pos = !pos_ref in
      let n_available = Bytes.length chunk - pos in
      let n_can_write = min n_to_write n_available in
      blit buf lt chunk pos n_can_write;
      pos_ref := pos + n_can_write;
      if n_can_write < n_to_write then begin
        buffer.chunks <- (
            Bytes.create (
              max (pos + n_can_write) (max 16 (n_to_write - n_can_write))),
            ref 0
          )::buffer.chunks;
        append_from (lt + n_can_write)
      end in
    append_from lt;
    buffer.len <- buffer.len + (rt - lt)

  let append_bytes = append_chars_from Bytes.blit

  let append_sub = append_chars_from String.blit

  let append buffer buf = append_sub buffer buf 0 (String.length buf)

end

let of_buffer buffer = fun buf lt rt -> Buffer.append_sub buffer buf lt rt

let write oc str = oc str 0 (String.length str)

let write_sub oc str lt rt = oc str lt rt
