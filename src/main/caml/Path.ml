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

include DisableGenericCompare

type t = string

let dir_sep = Filename.dir_sep

let of_string s = s

let to_string p = p

let join p q =
  if str_eq "" p then
    q
  else if StringUtil.ends_with p dir_sep then
    p ^ q
  else
    p ^ dir_sep ^ q

let join_str = join

let join_all paths =
  String.concat dir_sep
    (List.fold_right
      (fun path paths ->
        if str_eq path "" then
          paths
        else if StringUtil.ends_with path dir_sep then
          (String.sub path 0 ((String.length path) - (String.length dir_sep)))
          ::paths
        else
          path::paths)
      paths [])

let join_strs base components = join_all (base::components)

let dirname = Filename.dirname

let basename = Filename.basename

let prefix p = Filename.chop_extension (Filename.basename p)

let has_suffix suffix p = Filename.check_suffix p suffix

let is_absolute p = not (Filename.is_relative p)

let ls p =
  List.sort cmp_str (List.map (join_str p) (Array.to_list (Sys.readdir p)))

let exists p = Sys.file_exists p

let is_dir p = exists p && Sys.is_directory p

let is_file p = exists p && not (is_dir p) (* TODO: exclude symlinks, pipes *)

let canon ?(getcwd = Sys.getcwd) p =
  let sep = Filename.dir_sep in
  let sep_len = String.length sep in

  let p, parts =
    let split p =
      let n = String.length p in
      let rec walk i pos parts_rev =
        if i < 0 then
          (0, pos)::parts_rev
        else if i + sep_len <= pos
            && StringUtil.region_matches p i sep 0 sep_len then
          walk (i-1) i ((i + sep_len, pos)::parts_rev)
        else
          walk (i-1) pos parts_rev in
      walk (n - sep_len) n [] in
    let cwd = getcwd () in
    let is_p_absolute = is_absolute p in
    (* If absolute, attach the current working directory. *)
    let p' = if is_p_absolute then p else cwd ^ sep ^ p in
    p', split p' in
  (* Filter out "." and "" parts. *)
  let parts =
    let self = Filename.current_dir_name in
    let self_len = String.length self in
    List.filter
      (fun (s, e) ->
        not (
          (* Filter out "." parts. *)
          (e - s = self_len
           && StringUtil.region_matches p s self 0 self_len)
          (* Filter out empty parts. *)
          || e = s))
     parts in
  (* Fold ".." parts. *)
  let parts =
    let parent = Filename.parent_dir_name in
    let parent_len = String.length parent in
    (* [".."; "a"; ".."; "b"] -> ["b"]
       [""; ".."; ".."; "c"; "d"; ".."; "e"] -> [""; "c"; "e"] *)
    let rec fold_parent parts_tl parts_hd_rev = match parts_tl with
      | [] -> List.rev parts_hd_rev
      | ((s, e) as part)::parts_tl' ->
        fold_parent parts_tl'
          (if e - s = parent_len
              && StringUtil.region_matches p s parent 0 parent_len then
            (match parts_hd_rev with
              | []   -> []
              | _::x -> x)
          else
            part::parts_hd_rev) in
     fold_parent parts [] in
  (* TODO: we could lower-case if the FS requires it. *)
  (* TODO: Unix.readlink on all prefixes. *)
  if is_empty parts then
    sep
  else
    let length, n_parts = List.fold_left
      (fun (length, n_parts) (s, e) -> (length + e - s, n_parts + 1))
      (0, 0) parts in
    let out = Bytes.create (length + n_parts * sep_len) in
    let idx = List.fold_left
      (fun i (s, e) ->
        (* TODO: should this be here for the first part for windows? *)
        Bytes.blit_string sep 0 out i             sep_len;
        Bytes.blit_string p   s out (i + sep_len) (e - s);
        i + sep_len + e - s)
      0 parts in
    assert (idx = Bytes.length out);
    Bytes.to_string out

let rec mkdirs ?(perms=0o750) p = match p with
  | _ when exists p -> ()
  | "." | "" ->
    invalid_arg (Printf.sprintf "working directory `%s` does not exist" p)
  | _ -> (match basename p with
      | "" | "." | ".." -> mkdirs ~perms (dirname p)
      | _ ->
        let parent = dirname p in
        mkdirs ~perms parent;
        Unix.mkdir p perms)

let file_size p =
  let in_file = open_in p in
  try
    let size = in_channel_length in_file in
    close_in in_file;
    size
  with e ->
    (try close_in in_file with _ -> ());
    raise e

let read_to_string p =
  let in_file = open_in p in
  try
    let len = in_channel_length in_file in
    let str = Bytes.create len in
    really_input in_file str 0 len;
    close_in in_file;
    Bytes.to_string str
  with e ->
    (try close_in in_file with _ -> ());
    raise e

let read f p =
  let channel = open_in p in
  let result =
    try
      f (ByteInput.of_in_channel channel)
    with | error -> (close_in_noerr channel; raise error) in
  close_in channel;
  result

let write_with_channel ?(truncate=true) f p =
  let perm = 0o640 in
  let mode = [Open_wronly; Open_append; Open_creat; Open_binary] in
  let mode = if truncate then Open_trunc::mode else mode in
  let channel = open_out_gen mode perm p in
  let result = (
    try
      f channel
    (* Close noerr on failure so that test code that uses this to write
       a log still propagates the test failure with the full stack. *)
    with | error -> (close_out_noerr channel; raise error)) in
  close_out channel;
  result

let write ?(truncate=true) f p =
  write_with_channel
    ~truncate (fun channel -> f (ByteOutput.of_out_channel channel)) p

let compare = cmp_str

let stringer = Stringer.ctor "path" Stringer.string
