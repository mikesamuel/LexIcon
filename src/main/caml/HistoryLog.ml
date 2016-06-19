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

type t = {
  prefix      : string;
  (** A prefix like ["foo("] that precedes the JSON content. *)
  entries_rev : Encodable.t list;
  (** The log entries in reverse so append is efficient. *)
  suffix      : string;
  (** A suffix like [");"] that follows the JSON content. *)
  exec        : string list -> string * int;
  (** Executes commands. *)
  git_dir     : string;
  (** Directory relative to which git commands are executed. *)
  version     : string;
  (** Opaque identifier for revision control client state. *)
}

type reader = { read  : 'a . ((ByteInput.t  -> 'a) -> Path.t -> 'a) }
type writer = { write : 'a . ((ByteOutput.t -> 'a) -> Path.t -> 'a) }

let make ?(exec=ExecUtil.exec) ?(reader={read=Path.read}) log_path =
  let jsonp = reader.read (fun inp -> ByteInput.to_string inp) log_path in
  (* The format should be

       js_identifier("JSON CONTENT HERE");

     Just look for the parens bounding the call.
  *)
  let rec find str index (step : int -> int) matcher =
    if index < 0 || index >= String.length str then
      None
    else
      let ch = str.[index] in
      if matcher ch then
        Some index
      else
        find str (step index) step matcher in
  let n = String.length jsonp in
  (match (find jsonp 0     ((+)  1) ((=%) '('),
          find jsonp (n-1) ((+)~-1) ((=%) ')')) with
    | Some left_paren, Some right_paren when left_paren < right_paren ->
      (match (find jsonp (left_paren +1) ((+)  1) ((<=%) ' '),
              find jsonp (right_paren-1) ((+)~-1) ((<=%) ' ')) with
        | Some non_space_after_lparen, Some non_space_before_rparen ->
          let prefix = String.sub jsonp 0 non_space_after_lparen in
          let right_boundary = non_space_before_rparen + 1 in
          let suffix = String.sub jsonp right_boundary (n - right_boundary) in
          let log_json = String.sub jsonp non_space_after_lparen
            (right_boundary - non_space_after_lparen) in
          let log = Encodable.of_json ~source:(Path.to_string log_path)
            (ByteInput.of_string log_json) in
          (match log with
            | Encodable.Arr entries ->
              let git_dir, git_dir_status = exec [
                "git"; "--work-tree"; (Path.to_string log_path);
                "rev-parse"; "--git-dir";
              ] in
              if git_dir_status <> 0 then
                failwith (Printf.sprintf "Failed to determing git-dir for %s"
                            (Stringer.s Path.stringer log_path));
              let version, version_status = exec [
                "git"; "--git-dir"; git_dir; "rev-list"; "--max-count=1"; "HEAD"
              ] in
              if version_status <> 0 then
                failwith (Printf.sprintf "Failed to determing version for %s"
                            (Stringer.s Path.stringer log_path));
              let entries_rev = List.rev entries in
              { prefix; entries_rev; suffix; git_dir; version; exec }
            | _                     ->
              failwith (Printf.sprintf
                          ("JSONP log file %s contained %s,"
                           ^^ " not an array of log entries")
                          (Stringer.s Path.stringer log_path) log_json)
          )
        | _ ->
          failwith (Printf.sprintf "JSONP log file %s contains no JSON body"
                      (Stringer.s Path.stringer log_path))
      )
    | None, _ ->
      failwith (Printf.sprintf "JSONP Log file %s contains no '('"
                  (Stringer.s Path.stringer log_path))
    | _ ->
      failwith (Printf.sprintf "JSONP Log file %s contains no ')' after a '('"
                  (Stringer.s Path.stringer log_path))
  )


let update ?(clock=Unix.time) log entry =
  let hostname, hostname_status = log.exec ["hostname"] in
  if hostname_status <> 0 then failwith "failed to exec hostname";

  let datetime = match Unix.gmtime (clock ()) with
    | { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } ->
      let year = tm_year + 1900 in
      let mon  = tm_mon  + 1 in
      Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02d"
        year mon tm_mday tm_hour tm_min tm_sec in

  let entry_with_metadata = Encodable.Rel [
    Encodable.Str "version",  Encodable.Str log.version;
    Encodable.Str "hostname", Encodable.Str hostname;
    Encodable.Str "datetime", Encodable.Str datetime;
    Encodable.Str "entry",    entry;
  ] in
  let prior_entries_rev = match log.entries_rev with
    | (Encodable.Rel pairs)::tl ->
      let rec has_same_version pairs = match pairs with
        | (Encodable.Str "version", Encodable.Str v)::_  -> str_eq v log.version
        | _                                         ::tl -> has_same_version tl
        | []                                             -> false in
      if has_same_version pairs then tl else log.entries_rev
    | log_entries_rev                                    -> log_entries_rev in
  { log with entries_rev=(entry_with_metadata::prior_entries_rev) }


let commit { prefix; entries_rev; suffix; _ } out =
  ByteOutput.write out prefix;
  let sink, flush =
    Stringer.indenter ~indent:2 (* TODO: compute from suffix of prefix *)
      (ByteOutput.write out) in
  Encodable.json_stringer sink (Encodable.Arr (List.rev entries_rev));
  flush ();
  ByteOutput.write out suffix


let update_in_place
    ?(clock =Unix.time)
    ?(exec  =ExecUtil.exec)
    ?(reader={read =Path.read })
    ?(writer={write=(fun f p -> Path.write f p)})
    log_path
    entry =
  let log = make ~exec ~reader log_path in
  let log' = update ~clock log entry in
  ignore (writer.write (commit log') log_path)
