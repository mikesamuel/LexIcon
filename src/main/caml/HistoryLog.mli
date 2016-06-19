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

(** Operations on persistent JSON log files that enable our test suite to
    associate benchmark results with versions of code in the GIT repository
    for this project.

    See trends/README for the details of the log file format.

    If parameter defaults are used, then this module requires that the log
    files that it operates on fall within (ignoring symlinks) a [git]
    repository and that [git] is available on the executable search path.

    [git --work-tree=$(dirname <log_file_path>) rev-parse --git-dir]
    is used to find the git directory to query and then other commands
    are run via [git --git-dir=<git-dir> ...].
*)


type t
(** A persistent JSONP log. *)

type reader = { read  : 'a . ((ByteInput.t  -> 'a) -> Path.t -> 'a) }
type writer = { write : 'a . ((ByteOutput.t -> 'a) -> Path.t -> 'a) }


val make :
     ?exec   : (string list -> string * int)
  -> ?reader : reader
  -> Path.t
  -> t
(** [update_log ~exec ~read log_path]
    reads the persistent JSONP log whose content is
    [read ByteInput.toString log_path] and uses [Path.dirname log_path]
    to extract log entry metadata from the local
    repository by executing commands using [exec]. *)


val update :
     ?clock : (unit        -> float)
  -> t
  -> Encodable.t
  -> t
(** [update ~clock log entry] is [log] but with one change:
    {ul
     {li If the log has no entries then [entry] is appended.}
     {li otherwise, if the hash of the git repository
         (as reported by [eval \["git"; "describe"; ...\]])
         is the same as the ["git_hash"] property of the last
         entry, then the last entry is replaced with [entry].}
     {li otherwise, [entry] is appended.}
    }

    @param clock must return a seconds-since-epoch value suitable for use with
          {!Unix.gmtime}.
*)


val commit :
     t
  -> ByteOutput.t
  -> unit
(** [commit log] writes the log to the given output stream. *)


val update_in_place :
     ?clock  : (unit        -> float)
  -> ?exec   : (string list -> string * int)
  -> ?reader : reader
  -> ?writer : writer
  -> Path.t
  -> Encodable.t
  -> unit
(** [update_in_place ~clock ~exec ~read log_path entry] reads the log file,
    at [log_path], updates it, and writes it out.

    No file-locking is done, so this may be subject to race-conditions if
    multiple threads are trying to update the same log file concurrently.

    @param clock returns a seconds-since-epoch value suitable for use with
          {!Unix.gmtime}.
    @param exec executes command-line commands.
    @param log_file is the location of the log file used to determine the
           appropriate git directory to query for hashes.
    @param entry a JSON encodable.
*)
