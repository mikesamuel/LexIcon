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

let sprintf = Printf.sprintf

type input =
  | Data of StrCursor.t
  | Interrupt

type ('meta, 'operator) event =
  | Match  of input
  | Push   of 'meta Decoder.t
  | Pop    of 'meta Enc.t
  | Enter  of 'operator
  | Exit   of 'operator * Var.Pred.t
  | VarDef of Var.Name.t
  | VarSet of Var.Name.t * Var.Value.t
  | VarPop of Var.Name.t

type input_bounds = {
  pos     : input list;
  start   : int;
  restart : int option;
  current : int;
}

type ('m, 'operator) t = {
  state      : ('m, 'operator) PegParser.State.t;
  bounds     : input_bounds;
  events_rev : ('m, 'operator) event list;
}

let input_stringer out x = match x with
  | Data c    -> StrCursor.range_stringer out c
  | Interrupt -> out "interrupt"

let compact_input_stringer out x = match x with
  | Data c    -> out (sprintf "%d" (StrCursor.as_index c))
  | Interrupt -> out "/"

let input_repr_stringer out x = match x with
  | Data c    -> Stringer.string out (StrCursor.substr c)
  | Interrupt -> out "/"

let bounds_stringer out { start; restart; current; pos } = Stringer.orec4
  "start"   Stringer.int                   ~-1
  "restart" (Stringer.option Stringer.int) None
  "current" Stringer.int                   ~-1
  "pos"     (Stringer.list input_stringer) []
  out
  (start, restart, current, pos)

let event_stringer op_stringer out e =
  let abbrev = Stringer.abbrev in
  let ctor = Stringer.ctor in
  let op_and_pred_stringer = Stringer.tup2 op_stringer Var.Pred.stringer in
  match e with
    | Match  i      ->
      ctor "Match" input_stringer out i;
      (match i with
        | Data c ->
          let out_abbrev = Stringer.abbrev_sink out in
          out_abbrev "(*";
          (fun out s -> Stringer.string out (StrCursor.substr s)) out_abbrev c;
          out_abbrev "*)"
        | Interrupt -> ())
    | Push   d      -> ctor "Push"   (abbrev Decoder.stringer) out d
    | Pop    e      -> ctor "Pop"    (abbrev Enc.stringer)     out e
    | Enter  o      -> ctor "Enter"  op_stringer               out o
    | Exit   (o, p) -> ctor "Exit"   op_and_pred_stringer      out (o, p)
    | VarDef n      -> ctor "VarDef" Var.Name.stringer         out n
    | VarPop n      -> ctor "VarPop" Var.Name.stringer         out n
    | VarSet (n, v) -> ctor "VarSet"
      (Stringer.tup2 Var.Name.stringer Var.Value.stringer) out (n, v)

let stringer ?(id_to_name=PegParser.State.default_id_to_name)
    op_stringer out { state; bounds; events_rev } =
  PegParser.State.ctor_name_stringer out state;
  let state_stringer =
    PegParser.State.repr_stringer ~id_to_name:id_to_name op_stringer in
  let state_stringer = match state with
    | PegParser.State.Token    _ -> state_stringer
    | _                          -> Stringer.abbrev state_stringer in
  state_stringer out state;
  out "@";
  Stringer.list input_repr_stringer out bounds.pos;
  if not (is_empty events_rev) then begin
    out ":";
    Stringer.list (event_stringer op_stringer) out (List.rev events_rev)
  end

type ('m, 'operator) logger = {
  checkpoint_stack : ('m, 'operator) t list -> unit;
  token_consumed   : 'm Regex.t -> input list -> unit;
  event_pushed     : ('m, 'operator) event -> unit;
}

let noop_logger = {
  checkpoint_stack = ignore;
  token_consumed   = (fun _ _ -> ());
  event_pushed     = ignore;
}

module Path = struct
  type ('meta, 'operator) runtime = ('meta, 'operator) t

  type ('meta, 'operator) t = {
    stack         : ('meta, 'operator) runtime list;
    longest_match : int;
    inputs        : input list;
  }

  let top { stack; _ } = match stack with
    | []    -> invalid_arg "empty"
    | hd::_ -> hd.state

  let is_interrupted_stack stack = (match stack with
    | [] -> false
    | { bounds = { pos; _ }; _ }::_ ->
      let rec has_interrupt pos = match pos with
        | (Data c)::tl when StrCursor.is_empty c ->
          has_interrupt tl
        | Interrupt::_                           -> true
        | _                                      -> false in
      has_interrupt pos)

  let is_interrupted ({ stack; _ }) = is_interrupted_stack stack

  let commit ({ stack; _ } as path) =
    let stack' = List.map
      (fun s -> match s with
        | { state = PegParser.State.Union (m, _); _ } ->
          { s with state = PegParser.State.Union (m, []) }
        | _                                 -> s)
      stack in
    { path with stack = stack' }

  let resume ({ stack; _ } as path) =
    (* Strip empty cursors followed by an interrupt off top element. *)
    let stack' = match stack with
      | ({ bounds = { pos; _ }; events_rev; _ } as hd)::tl ->
        let rec find_interrupt pos events_rev' = match pos with
          | (Data c as hd)::tl ->
            if StrCursor.is_empty c then
              find_interrupt tl ((Match hd)::events_rev')
            else
              None
          | Interrupt::tl      -> Some (tl, events_rev')
          | _                  -> None in
        (match find_interrupt pos events_rev with
          | Some (pos', events_rev') ->
            {
              hd with bounds = { hd.bounds with pos = pos' };
                      events_rev=events_rev';
            }::tl
          | None -> stack)
      | _ -> stack in
    { path with stack = stack' }

  let in_embedded_extent stack =
    List.exists
      (fun el -> match el with
        | { state=PegParser.State.Embed _; events_rev; _ } ->
          List.for_all
            (fun ev -> match ev with | Push _ -> false | _ -> true)
            events_rev
        | _ -> false)
      stack

end
