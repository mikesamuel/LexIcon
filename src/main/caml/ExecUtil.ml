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

include DisableGenericCompare


module Buf = ByteOutput.Buffer


let _log s = Printf.printf "%s\n" s; flush stdout


let make_command_text argv =
  let buf = Buf.make
    ~size:(List.fold_left (fun n s -> n + 1 + String.length s) 16 argv)
    () in
  List.iter
    (fun s ->
      Buf.append buf "'";
      let rec esc_str s i p n =
        if i = n then
          Buf.append_sub buf s p (i - p)
        else if chr_eq s.[i] '\'' then begin
          Buf.append_sub buf s p (i - p);
          Buf.append buf "'\"'\"'";
          esc_str s (i + 1) (i + 1) n
        end else
          esc_str s (i + 1) p n in
      esc_str s 0 0 (String.length s);
      Buf.append buf "' ";
    )
    argv;
  Buf.to_string buf


(* remove any trailing newline. *)
let chomp buf =
  let buf_len = Buf.length buf in
  let output_len =
    if buf_len <> 0 && chr_eq (Buf.char_at buf (buf_len - 1)) '\n' then
      if buf_len <> 1 && chr_eq (Buf.char_at buf (buf_len - 2)) '\r' then
        buf_len - 2
      else
        buf_len - 1
    else
      buf_len in
  ByteOutput.Buffer.sub buf 0 output_len


let exec argv =
  let command_text = make_command_text argv in
  let out_buf = Buf.make () in
  let rec drain =
    let buffer_len = 1024 in
    let buffer = Bytes.make buffer_len '\x00' in
    fun in_stream ->
      let n_read = Pervasives.input in_stream buffer 0 buffer_len in
      Buf.append_bytes out_buf buffer 0 n_read;
      if n_read <> 0 then
        drain in_stream in

  let process_stdout = Unix.open_process_in command_text in
  begin
    try
      drain process_stdout;
    with
      | x ->
        ignore (Unix.close_process_in process_stdout);
        raise x
  end;

  match Unix.close_process_in process_stdout with
    | Unix.WEXITED   ret_code -> chomp out_buf, ret_code
    | Unix.WSIGNALED signal   ->
      failwith (Printf.sprintf "%s killed by signal %d"
                  (String.concat " " argv) signal)
    | Unix.WSTOPPED  signal   ->
      failwith (Printf.sprintf "%s stopped by signal %d"
                  (String.concat " " argv) signal)


let assert_command ?(verbose=false) ?(exit_code=0) argv = begin
  let dump_command result =
    Printf.printf "Command %s: %s\n"
      result
      (String.concat " \\\n    " (List.map String.escaped argv)) in
  let stdout, actual_exit_code =
    try
      exec argv
    with | (Failure _ as failure) ->
      dump_command "failed";
      raise failure in
  if actual_exit_code <> exit_code then begin
    dump_command (Printf.sprintf "exited with code %d" actual_exit_code);
    Printf.printf "\n\n%s\n" stdout;
    failwith ((List.hd argv) ^ " failed")
  end else if verbose then begin
    dump_command "passed";
    Printf.printf "\n\n%s\n" stdout;
  end;
end
