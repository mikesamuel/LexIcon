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

include DisableGenericCompare

module Enc = Encodable

type 'a t = {
  unpack    : Encodable.t -> 'a;
  stringify : Stringer.sink -> unit;
}

exception Missing_config
  of string list * (Stringer.sink -> unit) * string list
exception Illegal_config
  of string list * (Stringer.sink -> unit) * Enc.t
exception Unused_config
  of string list * (Stringer.sink -> unit) * string list

type 'a prop = {
  key             : string;
  doc             : string;
  stringify_value : Stringer.sink -> unit;
  value_setter    : Encodable.t -> 'a -> 'a;
  default_setter  : ('a -> 'a) option;
}

module StringMap = MapUtil.StringMap
module StringSet = SetUtil.StringSet

let attach_key key unpacker e = begin
  try
    unpacker e
  with
    | Missing_config (keys, stringify, missing) ->
      raise (Missing_config (key ()::keys, stringify, missing))
    | Illegal_config (keys, stringify, enc) ->
      raise (Illegal_config (key ()::keys, stringify, enc))
    | Unused_config  (keys, stringify, unu) ->
      raise (Unused_config  (key ()::keys, stringify, unu))
end
(** Used to build up a key list while unwinding the stack. *)

let word_break out s =
  let module CharCategory = struct
    type t = Word | Space | Other
    include MakeSimpleCmp (struct type comparable = t end)
    let of_char ch =
      if ch <=% ' ' then
        Space
      else if ('a' <=% ch && ch <=% 'z' || 'A' <=% ch && ch <=% 'Z'
               || '0' <=% ch && ch <=% '9'
               || ch =% '_' || ch =% '-' || ch =% '.') then
        Word
      else
        Other
  end in
  let n = String.length s in
  let maybe_emit p i =
    if p < i then begin
      if i - p = 1 && chr_eq s.[p] ' ' then
        ()  (* Let stringer break lines and handle inserting whitespace *)
      else
        out (String.sub s p (i - p))
    end in
  let rec brk k p i =
    if i = n then begin
      maybe_emit p i
    end else
      let kind = CharCategory.of_char s.[i] in
      let p' =
        if CharCategory.equal k kind then
          p
        else begin
          maybe_emit p i;
          i
        end in
      brk kind p' (i + 1) in
  if n <> 0 then
    brk (CharCategory.of_char s.[0]) 0 1

let prop ~key ~doc ?(default=None) unpacker setter = begin
  let stringify_value out =
    out "(";
    (match default with
      | Some (x, stringer) -> out "default"; stringer out x; out ":"
      | None -> ());
    word_break out doc;
    out ":";
    unpacker.stringify out;
    out ")" in
  let value_setter e = setter (unpacker.unpack e) in
  let default_setter = Opt.map (fun (x, _) -> setter x) default in
  {
    key;
    doc;
    stringify_value;
    value_setter;
    default_setter;
  }
end

let force unpacker = {
  unpacker with unpack = (fun e -> match unpacker.unpack e with
    | Some x -> x
    | None   -> raise (Illegal_config ([], unpacker.stringify, e)));
}

let option unpacker = {
  unpacker with unpack = (fun e ->
    try
      Some (unpacker.unpack e)
    with
      | Illegal_config (_, _, e') when Encodable.equal e e' ->
        None
  );
}

let any options fallback = match options with
  | [] -> fallback
  | _  -> begin
    let stringify out =
      out "(";
      List.iter (fun unpacker -> unpacker.stringify out; out "|") options;
      fallback.stringify out;
      out ")" in
    let unpack e =
      let rec try_each untried = match untried with
        | []      -> fallback.unpack e
        | hd::tl -> (match hd.unpack e with
            | Some x -> x
            | None   -> try_each tl) in
      try_each options in
    { stringify; unpack }
  end

let manifold unpacker fold finish empty =
  let stringify out =
    out "[";
    unpacker.stringify out;
    out ", ";
    out "...";
    out "]" in
  let unpack e = match e with
    | Encodable.Arr ls ->
      let _, x =
        List.fold_left
          (fun (i, x) el ->
            i + 1,
            fold x (attach_key (fun _ -> string_of_int i) unpacker.unpack el))
          (0, empty) ls in
      finish x
    | _ ->
      raise (Illegal_config ([], stringify, e)) in
  { stringify; unpack }

let many unpacker = manifold unpacker (fun ls_rev el -> el::ls_rev) List.rev []

let obj props ctor = begin
  (* Map property names to handlers. *)
  let key_to_prop = List.fold_left
    (fun m x -> StringMap.add_no_override x.key x m)
    StringMap.empty props in
  (* Map property names to indices so we can sort properties to deliver
     values to properties in the order they are declared which makes it
     easier to debug since required property failures show up in the same order
     as in the help text, and makes it easier to write property lists that merge
     multiple properties into one value since property handlers only need to
     worry about clobbering by later property handlers. *)
  let key_to_index, _ = List.fold_left
    (fun (m, i) x -> (StringMap.add x.key i m), (i + 1))
    (StringMap.empty, 0) props in
  (* Required  *)
  let required_keys = List.fold_left
    (fun s x -> if is_none x.default_setter then StringSet.add x.key s else s)
    StringSet.empty props in

  let stringify_obj out =
    out "{";
    ignore (
      StringMap.fold
        (fun key prop needs_comma ->
          if needs_comma then begin out ","; out "\n" end;
          Stringer.string out key;
          out ":";
          prop.stringify_value out;
          true)
        key_to_prop false
    );
    out "}" in

  {
    unpack = (fun e -> match e with
      | Enc.Rel ls ->
        (* Sort by property index for reasons described above. *)
        let ls = List.stable_sort
          (fun (a, _) (b, _) ->
            let index_of x = match x with
              | Encodable.Str s -> StringMap.find_opt s key_to_index
              | _               -> None in
            match index_of a, index_of b with
              | Some i, Some j -> i - j
              | Some _, None   -> ~-1
              | None,   Some _ -> 1
              | None,   None   -> Encodable.compare a b)
          ls in
        (* Check that we have all the required properties. *)
        let missing = List.fold_left
          (fun req (k, _) -> match k with
            | Encodable.Str key -> StringSet.remove key req
            | _                 -> req)
          required_keys ls in
        if not (StringSet.is_empty missing) then
          raise (Missing_config (
            [], stringify_obj, StringSet.elements missing));

        (* Match properties with handlers and curry the corresponding values
           with the property setters. *)
        let unused_rev, setters_rev = List.fold_left
          (fun (unused_rev, setters_rev) (key_enc, value) -> match key_enc with
            | Encodable.Str key_str ->
              (match StringMap.find_opt key_str key_to_prop with
                | None -> key_enc::unused_rev, setters_rev
                | Some prop ->
                  let setter =
                    attach_key (fun _ -> key_str) prop.value_setter value in
                  unused_rev, setter::setters_rev
              )
            | _ -> key_enc::unused_rev, setters_rev
          )
          ([], []) ls in
        (match unused_rev with
          | _::_ ->
            raise (Unused_config (
              [], stringify_obj,
              List.rev_map
                (fun x -> match x with
                  | Encodable.Str s -> s
                  | _ -> Stringer.s Encodable.json_stringer x)
                unused_rev))
          | []   ->
            (* Now that we have all the setters, create a value and apply the
               setters in preferred property order. *)
            List.fold_right
              (fun setter x -> setter x)
              setters_rev (ctor ())
        )
      | e ->
        raise (Illegal_config ([], stringify_obj, e))
    );
    stringify = stringify_obj
  }
end

let relation key_unpacker value_unpacker =
  let stringify out =
    out "{"; key_unpacker.stringify out; out ":"; value_unpacker.stringify out;
    out ","; out "..."; out "}" in
  let unpack add empty e = match e with
    | Encodable.Rel ls ->
      List.fold_left
        (fun x (key, value) ->
          let unpacked_key = key_unpacker.unpack key in
          let unpacked_value = attach_key
            (fun _ -> Stringer.s Encodable.json_stringer key)
            value_unpacker.unpack value in
          add unpacked_key unpacked_value x)
        empty ls
    | _ ->
      raise (Illegal_config ([], stringify, e)) in
  fun add empty -> { stringify; unpack=unpack add empty }

let string = {
  stringify = (fun out -> out "\"...\"");
  unpack = (fun e -> match e with
    | Encodable.Str s -> Some s
    | _               -> None);
}
let float = {
  stringify = (fun out -> out "<number>");
  unpack = (fun e -> match e with
    | Encodable.Num n -> Some n
    | _               -> None);
}
let int = {
  stringify = (fun out -> out "<int>");
  unpack = (fun e -> match e with
    | Encodable.Int i -> Some i
    | _               -> None);
}
let null = {
  stringify = (fun out -> out "null");
  unpack = (fun e -> match e with
    | Encodable.Nil -> Some ()
    | _             -> None);
}
let bool = {
  stringify = (fun out -> out "<bool>");
  unpack = (fun e -> match e with
    | Encodable.Bool b -> Some b
    | _                -> None);
}

let literal input output = {
  stringify = (fun out -> Encodable.json_stringer out input);
  unpack = (fun e ->
    if Encodable.similar e input then
      Some output
    else
      None);
}

let unpack unpacker = unpacker.unpack

let map f unpacker = {
  unpacker with
  unpack    = (fun e -> f (unpacker.unpack e));
}

let filter p unpacker = {
  unpacker with
  unpack = (fun e ->
    let result = unpacker.unpack e in
    if p result then
      result
    else
      raise (Illegal_config ([], unpacker.stringify, e)))
}

let with_stringify stringify up = { up with stringify }

let stringer out { stringify; _ } = stringify out
