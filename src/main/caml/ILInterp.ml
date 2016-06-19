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

(**
   An interpreter for the PEG intermediate-language used for debugging
   backends by providing a simple implementation of the IL for comparison.
*)

include DisableGenericCompare

module Actual = Interpreter.Actual
module Buffer = ByteOutput.Buffer
module CUK    = CodeUnitKind
module E      = Encodable
module Formal = Signature.Formal
module SCV    = ScalarCharValue

type 'a reference = 'a Interpreter.Reference.t = {
  get : unit -> 'a;
  set : 'a -> unit;
}

let sprintf = Printf.sprintf

exception PanicExn

let junk inner outer msg =
  let node_stringer = OpTree.node_stringer DecoderOperator.stringer in
  failwith (sprintf "Junk\n\t%s\nin\n\t%s\n%s"
              (Stringer.s node_stringer inner)
              (Stringer.s node_stringer outer)
              msg)

let encode_op_tree_handler _ _ buffer_opt op_tree =
  match op_tree, buffer_opt with
    | [],                Some _      -> true
    | [OpTree.Leaf str], Some buffer ->
      Buffer.append buffer str;
      true
    | _,                 None        -> false
    | _,                 Some _      -> false

let decode_op_tree_handler data_kind = OpTree.(
  let rec flatten t' ls = match ls with
    | []                  -> t'
    | (Embed (_, ls))::tl -> flatten (flatten t' tl) ls
    | hd             ::tl -> hd::(flatten t' tl) in
  let rec filter_and_decode t =
    let decode_node n = match n with
      | Op    (DecoderOperator.CreateNullValue,       _)        ->
        E.Nil
      | Op    (DecoderOperator.CreateBooleanValue b,  _)        ->
        E.Bool b
      | Op    (DecoderOperator.CreateNumericValue ns, [Leaf s]) ->
        (match NumberSystem.decode_number ns s 0 (String.length s) with
          | NumberSystem.Int   i -> E.Int i
          | NumberSystem.Float f -> E.Num f
        )
      | Op    (DecoderOperator.CreateStringValue,     ls)       ->
        let buffer = Buffer.make () in
        List.iter
          (fun x -> match x with
            (* We don't preserve Char operators in DecoderILBridge's
               op_disposition. *)
            | Op   (DecoderOperator.AppendCurrent,     [Leaf s])
            | Op   (DecoderOperator.AppendChars   s,   _       ) ->
              Buffer.append buffer s
            | Op   (DecoderOperator.AppendScalar  ns,  [Leaf s]) ->
              let code_unit = CodeUnit.of_int (
                NumberSystem.decode_integer ns s 0 (String.length s)
              ) in
              CUK.emit data_kind code_unit buffer
            | Leaf s -> Buffer.append buffer s
            | _      -> junk x n "non-char in string"
          )
          ls;
        E.Str (Buffer.to_string buffer)
      | Op    (DecoderOperator.CreateArrayValue,      ls) ->
        let els_rev = List.fold_left
          (fun els_rev x -> match x with
            | Op   (DecoderOperator.StoreArrayElement, ls) ->
              (match filter_and_decode ls with
                | Some array_element -> array_element::els_rev
                | None               -> E.Nil::els_rev)
            | Leaf _ -> els_rev
            | _      -> junk x n "non-element in array"
          )
          [] ls in
        E.Arr (List.rev els_rev)
      | Op    (DecoderOperator.CreateRelationValue,   ls) ->
        let key   = ref None in
        let value = ref None in
        let entries_rev = List.fold_left
          (fun entries_rev x -> match x with
            | Op (DecoderOperator.StoreKey   as o, ls)
            | Op (DecoderOperator.StoreValue as o, ls) ->
              let v = (match filter_and_decode ls with
                | Some array_element -> array_element
                | None               -> E.Nil) in
              let loc, other_loc = match o with
                | DecoderOperator.StoreKey -> key,   value
                | _                        -> value, key in
              if is_none !loc then
                loc := Some v
              else
                failwith "dupe key or value";
              (match !other_loc with
                | Some _ ->
                  let entry = (Opt.require !key, Opt.require !value) in
                  key   := None;
                  value := None;
                  entry::entries_rev
                | None   -> (* wait *) entries_rev)
            | _ -> junk x n "non-key/value in relation"
          ) [] ls in
        (match !key, !value with
          | None,   None   -> ()
          | Some k, None   ->
            failwith ("orphaned key " ^ (Stringer.s E.json_stringer k))
          | _,      Some v ->
            failwith ("orphaned val " ^ (Stringer.s E.json_stringer v))
        );
        E.Rel (List.rev entries_rev)
      | Leaf  _
      | Embed _
      | Op    (DecoderOperator.Char,                  _)
      | Op    (DecoderOperator.CreateNumericValue _,  _)
      | Op    (DecoderOperator.StoreArrayElement,     _)
      | Op    (DecoderOperator.StoreKey,              _)
      | Op    (DecoderOperator.StoreValue,            _)
      | Op    (DecoderOperator.AppendCurrent,         _)
      | Op    (DecoderOperator.AppendChars _,         _)
      | Op    (DecoderOperator.AppendScalar _,        _) ->
        failwith (
          sprintf
            "Bad op_node %s"
            (Stringer.s (OpTree.node_stringer DecoderOperator.stringer) n)
        ) in
    match List.filter (fun n -> match n with Leaf _ -> false | _ -> true) t with
      | [root] -> Some (decode_node root)
      | _      -> None in
  fun _ out t -> match filter_and_decode (flatten [] t) with
    | Some (Encodable.Str decoded as x) ->
      (match out with
        | Some buf -> Buffer.append buf decoded
        | None     -> ());
      PegResult.Parsed x
    | Some x                            -> PegResult.Parsed x
    | None                              -> PegResult.Malformed ("", 0)
)

(* Use to handle push-back markers and match start and end markers. *)
module PbTree = struct
  type t =
    | Text     of string
    | Marker   of EvMarker.t
    | LR       of t list
    | Pushback of t list

  let rec stringer out x = match x with
    | Text     s  -> Stringer.ctor "Text"     Stringer.string          out s
    | Marker   m  -> Stringer.ctor "Marker"   EvMarker.stringer        out m
    | LR       ls -> Stringer.ctor "LR"       (Stringer.list stringer) out ls
    | Pushback ls -> Stringer.ctor "Pushback" (Stringer.list stringer) out ls
end


module Debugger = struct
  type env = (Label.t -> unit Stringer.t -> unit) -> unit

  type t = {
    log  : (string -> unit);
    start_stmt : (Label.t -> Scope.F.Idx.t -> int list -> env -> unit);
    end_stmt   : (Label.t -> Scope.F.Idx.t -> int list -> env -> bool -> unit);
  }

  let default = {
    log        = ignore;
    start_stmt = (fun _ _ _   -> ignore);
    end_stmt   = (fun _ _ _ _ -> ignore);
  }
end


type input_buffer = string

type output_buffer = Buffer.t

type input_cursor = {
  buffer : input_buffer;
  pos    : int reference;
  cuk    : CUK.t;
}

type value =
  | Uninitialized
  | OutputBuffer of output_buffer
  | InputCursor  of input_cursor
  | Snapshot     of int
  | CodeUnit     of CodeUnit.t * CUK.t
  | EnumValue    of unit Var.Domain.t * Var.Value.t option
  | ShallowPtr   of value reference
  | Match        of (int option * int) option
  | Bool         of bool
  | DomainData   of Encodable.t
  | ArrCursor    of Encodable.t list * int ref
  | RelCursor    of (Encodable.t * Encodable.t) list * int ref

let value_stringer out v = match v with
  | Uninitialized          -> out "Uninitialized"
  | OutputBuffer  _        -> out "OutputBuffer"
  | InputCursor   {pos; _} ->
    Stringer.ctor "InputCursor" Stringer.int out (pos.get ())
  | Snapshot      i        -> Stringer.ctor "Snapshot" Stringer.int      out i
  | CodeUnit      (c, k)   ->
    Stringer.ctor "CodeUnit" (Stringer.tup2 CodeUnit.stringer CUK.stringer)
      out (c, k)
  | EnumValue     (d, v)   ->
    Stringer.ctor "EnumValue"
      (Stringer.tup2 Var.Domain.stringer (Stringer.option Var.Value.stringer))
      out (d, v)
  | ShallowPtr    _        -> out "ShallowPtr"
  | Match         _        -> out "Match"
  | Bool          b        -> Stringer.ctor "Bool"     Stringer.bool     out b
  | DomainData    d        ->
    Stringer.ctor "DomainData" Encodable.stringer out d
  | ArrCursor     (_, i)   ->
    Stringer.ctor "ArrCursor" Stringer.int out !i
  | RelCursor     (_, i)   ->
    Stringer.ctor "RelCursor" Stringer.int out !i

let rec value_log_stringer out v = match v with
  | Uninitialized        -> out "[uninit]"
  | OutputBuffer  b      -> out "out"; Stringer.string out (Buffer.to_string b)
  | InputCursor   c      ->
    let { buffer; pos; _ } = c in
    out "icur";
    Stringer.string out buffer;
    out "@";
    Stringer.int out (pos.get ())
  | Snapshot      i      -> out "cur"; Stringer.int out i
  | CodeUnit      (c, _) -> CodeUnit.stringer out c
  | EnumValue     (_, v) -> (Stringer.option Var.Value.stringer) out v
  | ShallowPtr    v      -> out "&"; value_log_stringer out (v.get ())
  | Match         m      -> (match m with
      | None        -> out "no"; out "match"
      | Some (l, r) ->
        out "match";
        Stringer.tup2 (Stringer.option Stringer.int) Stringer.int out (l, r)
  )
  | Bool          b      -> Stringer.bool      out b
  | DomainData    d      -> Encodable.stringer out d
  | ArrCursor     (_, i) -> Stringer.ctor "ArrCursor" Stringer.int out !i
  | RelCursor     (_, i) -> Stringer.ctor "RelCursor" Stringer.int out !i

type cell = {
          label : Label.t;
          typ   : IL.ltype;
  mutable value : value;
}

let encode_marker_onto op_table =
  let tmp_buf = Bytes.make 4 '\x00' in
  let encode_int i buf =
    let n_bytes = Utf8.encode_onto tmp_buf 0 (Unicode.i2uni i) in
    Buffer.append_bytes buf tmp_buf 0 n_bytes in
  fun m buf -> match m with
    | EvMarker.StartUserOp (i,c) -> Buffer.append buf "\xff"; encode_int i buf;
                                    Hashtbl.replace op_table i c
    | EvMarker.PushEncoder h     -> Buffer.append buf "\xfe"; encode_int h buf
    | EvMarker.EndUserOp         -> Buffer.append buf "\xfd"
    | EvMarker.CancelUserOp      -> Buffer.append buf "\xfc\x00"
    | EvMarker.StartLR           -> Buffer.append buf "\xfc\x01"
    | EvMarker.EndLR             -> Buffer.append buf "\xfc\x02"
    | EvMarker.StartPushback     -> Buffer.append buf "\xfc\x03"
    | EvMarker.EndPushback       -> Buffer.append buf "\xfc\x04"
    | EvMarker.PopEncoder        -> Buffer.append buf "\xfc\x05"

let decode_marker_from op_table buf offset =
  if offset < String.length buf then
    match buf.[offset] with
      | '\xff' ->
        let cp, n_bytes = Utf8.decode buf (offset + 1) in
        let i = Unicode.uni2i cp in
        let cmt = Hashtbl.find op_table i in
        Some (EvMarker.StartUserOp (i, cmt),
              offset + 1 + n_bytes)
      | '\xfe' ->
        let cp, n_bytes = Utf8.decode buf (offset + 1) in
        Some (EvMarker.PushEncoder (Unicode.uni2i cp),
              offset + 1 + n_bytes)
      | '\xfd' -> Some (EvMarker.EndUserOp,     offset + 1)
      | '\xfc' ->
        let marker = match buf.[offset + 1] with
          | '\x00' -> EvMarker.CancelUserOp
          | '\x01' -> EvMarker.StartLR
          | '\x02' -> EvMarker.EndLR
          | '\x03' -> EvMarker.StartPushback
          | '\x04' -> EvMarker.EndPushback
          | '\x05' -> EvMarker.PopEncoder
          | _      -> failwith "invalid marker suffix" in
            Some (marker, offset + 2)
      | _      -> None
  else
    None

let rec interpret
  : 'o 'r
  .    ?op_stringer:'o Stringer.t
    -> ?debugger:Debugger.t
    -> (SideTable.t list -> int -> 'o)
    -> (Interpreter.t -> Buffer.t option -> 'o OpTree.t -> 'r)
    -> 'm CompiledPegs.t
    -> Label.t
    -> Actual.t list
    -> 'r PegResult.t
  = fun ?(op_stringer=Stringer.ignore) ?(debugger=Debugger.default)
        int_to_op interp_tree programs tool_label inputs ->

  let debug =
    if same debugger.Debugger.log ignore then
      ignore
    else
      (fun f -> debugger.Debugger.log (f ()))
  in

  debug (fun _ ->
    Printf.sprintf "ILInterp: tool_label=%s, inputs=%s\n"
      (Label.to_string tool_label)
      (Stringer.s (Stringer.list Actual.stringer) inputs)
  );

  let signature, IL.Program (globals, fns, start), side_tables, cuks =
    Label.Map.find tool_label programs in
  let int_to_op = int_to_op side_tables in


  let rec value_to_actual formal value = match value, formal with
    | DomainData   (E.Str s),      Formal.InputBuffer  -> Actual.InputBuffer  s
    | OutputBuffer b,              Formal.OutputBuffer -> Actual.OutputBuffer b
    | Snapshot     i,              Formal.InputLimit   -> Actual.InputLimit   i
    | InputCursor  {buffer;pos;_}, Formal.InputCursor  ->
      Actual.InputCursor (buffer, pos)
    | EnumValue    (_, Some v),    Formal.EnumValue _  -> Actual.EnumValue    v
    | ShallowPtr   p,              Formal.Reference t  ->
      Actual.Reference (
        Interpreter.Reference.map
          (value_to_actual t) (actual_to_value t) p
      )
    | DomainData   d,              Formal.DomainData   -> Actual.DomainData   d
    | CodeUnit     _,              _
    | Match        _,              _
    | Bool         _,              _
    | ArrCursor    _,              _
    | RelCursor    _,              _
    | Uninitialized,               _                   ->
      failwith "invalid actual parameter"
    | EnumValue    _,              _
    | ShallowPtr   _,              _
    | Snapshot     _,              _
    | InputCursor  _,              _
    | OutputBuffer _,              _
    | DomainData   _,              _                   ->
      failwith "type mismatch"
  and actual_to_value formal actual = match actual, formal with
    | Actual.OutputBuffer b,         Formal.OutputBuffer -> OutputBuffer b
    | Actual.DomainData   (E.Str s), Formal.InputBuffer  -> DomainData (E.Str s)
    | Actual.InputBuffer  b,         Formal.InputBuffer  -> DomainData (E.Str b)
    | Actual.InputCursor  (s, p),    Formal.InputCursor  ->
      InputCursor { buffer = s; pos = p; cuk = cuks.CodeUnitKinds.parse_kind }
    | Actual.InputLimit   i,         Formal.InputLimit   -> Snapshot i
    | Actual.EnumValue    _,         Formal.EnumValue _  ->
      failwith "TODO: lookup int value from headers"
    | Actual.Reference    r,         Formal.Reference t  ->
      ShallowPtr (
        Interpreter.Reference.map (actual_to_value t) (value_to_actual t) r
      )
    | Actual.DomainData   d,         Formal.DomainData   -> DomainData d
    | Actual.InputBuffer  _,         _
    | Actual.InputCursor  _,         _
    | Actual.InputLimit   _,         _
    | Actual.DomainData   _,         _
    | Actual.OutputBuffer _,         _
    | Actual.EnumValue    _,         _
    | Actual.Reference    _,         _                   ->
      invalid_arg (
        Printf.sprintf
          "type mismatch: actual %s is not of formal type %s"
          (Stringer.s Actual.stringer actual)
          (Stringer.s Formal.stringer formal)
      ) in


  let encode_marker_onto, decode_marker_from =
    let op_table = Hashtbl.create 16 in
    encode_marker_onto op_table, decode_marker_from op_table in

  let first_input_cursor = ref None in
  let longest_match = ref None in

  let capture_longest_match _ = begin
    (* Snapshot the first start cursor, so on a global failure, we
       can report the longest partial match. *)
    match !first_input_cursor with
      | Some (InputCursor { pos; _ }) ->
        let cpos = pos.get () in
        (match !longest_match with
          | None                 -> longest_match := Some cpos
          | Some x when x < cpos -> longest_match := Some cpos
          | Some _               -> ());
      | _                             -> ()
  end in

  let global_environment = Scope.G.fold
    (fun globals index label typ ->
      Scope.G.IdxMap.add index { label; typ; value = Uninitialized } globals
    )
    Scope.G.IdxMap.empty globals in

  let mut_count = ref 0 in

  let rec call fn_idx actual_values =
    match Scope.F.value fns fn_idx with
      | IL.Fn       (formals, arity, body) ->
        call_fn fn_idx actual_values formals arity body
      | IL.Override _                         -> true
      | IL.Extern   (_, extern_tool_label, _) ->
        let extern_sig, _, _, { CodeUnitKinds.data_kind; _ } =
          CompiledPegs.find extern_tool_label programs in
        let actuals = List.map2 value_to_actual
          extern_sig.Signature.formals actual_values in
        (match extern_sig.Signature.kind with
          | `Dec ->
            let result = interpret
              ~op_stringer:DecoderOperator.stringer
              ~debugger:debugger
              DecoderILBridge.int_to_op
              (decode_op_tree_handler data_kind)
              programs extern_tool_label actuals in
            (match result with
              | PegResult.Parsed (PegResult.Parsed (E.Str _)) ->
                (* Yay, the decoded output should have been left on the output
                   buffer. *)
                true
              | PegResult.Panic
              | PegResult.Parsed PegResult.Panic
              | PegResult.Parsed (PegResult.Parsed _) ->
                (* We expected to decode a string to a string but got
                   another variety of encodable. *)
                failwith "unexpected embedding result"
              | PegResult.Parsed (PegResult.Malformed (s, i))
              | PegResult.Malformed (s, i) ->
                let _ = s, i in   (* TODO: log to debugger *)
                false
            )
          | `Enc ->
            let result = interpret
              ~op_stringer:(fun _ _ -> failwith "op in encoder")
              ~debugger
              (fun _ i -> failwith (sprintf "unexpected op %d" i))
              (encode_op_tree_handler data_kind)
              programs extern_tool_label actuals
            in
            (match result with
              | PegResult.Parsed    true  -> true
              | PegResult.Parsed    false
              | PegResult.Malformed _
              | PegResult.Panic           ->
                failwith "failed to re-encode"
            )
          | k ->
            failwith (
              Printf.sprintf "TODO: implement extern calls for %s"
                (Stringer.s ToolKind.stringer k)))
  and call_fn fn_idx actuals formals arity body = begin
    debug (fun () ->
      sprintf "calling %s with %s"
        (Stringer.s Label.stringer (Scope.F.label fns fn_idx))
        (Stringer.s (Stringer.list value_stringer) actuals)
    );

    let type_mismatch e v t = begin
      let type_descriptor =
        let typable = match e with
          | `IE e -> Some (`IE e)
          | `EE e -> Some (`EE e)
          | _     -> None in
        (match typable with
          | None   -> ""
          | Some e ->
            ":" ^ (
              Stringer.s IL.SourceStringers.ltype
                (IL.typeof globals formals e)
            )
        ) in
      failwith (
        sprintf "Type mismatch %s%s yields %s, expected %s"
          (Stringer.s (IL.SourceStringers.any globals fns formals)  e)
          type_descriptor
          (Stringer.s value_stringer                                v)
          (Stringer.s (IL.SourceStringers.ltype)                    t)
      )
    end in

    let rec interp_stmt branch_addr locals s =
      let start_stmt, end_stmt = (match s with
        | IL.Block _ -> ignore, ignore  (* Do not log *)
        | _          ->
          let { Debugger.start_stmt; end_stmt; _ } = debugger in
          let debug_env () =
            let env_rev = Scope.L.IdxMap.fold
              (fun idx cell env_rev -> match cell.value with
                | Uninitialized -> env_rev
                | _             ->
                  (idx, fun out () -> value_log_stringer out cell.value)
                  ::env_rev
              )
              locals [] in
            fun f ->
              List.iter
                (fun (idx, stringer) -> f (Scope.L.label formals idx) stringer)
                (List.rev env_rev) in
          (
            (
              if same start_stmt Debugger.default.Debugger.start_stmt then
                ignore
              else
                fun () ->
                  start_stmt tool_label fn_idx branch_addr (debug_env ())
            ),
            (
              if same end_stmt   Debugger.default.Debugger.end_stmt   then
                ignore
              else
                end_stmt   tool_label fn_idx branch_addr (debug_env ())
            )
          )
      ) in
      start_stmt ();
      let passed = match s with
        | IL.Block (_, a, b)         ->
          if interp_stmt (0::branch_addr) locals a then
            (interp_stmt (1::branch_addr) locals b)
          else
            false
        | IL.Cond  (_, c)            ->
          let result = interp_pred locals c in
          if not result then begin
            debug (fun () ->
              sprintf "Failed guard %s"
                (Stringer.s (IL.SourceStringers.predicate globals formals) c)
            );
          end;
          result
        | IL.Alt   (_, default, alt) ->
          (  interp_stmt (0::branch_addr) locals default)
          || interp_stmt (1::branch_addr) locals alt
        | IL.Try   (_, body, recover)->
          (interp_stmt (0::branch_addr) locals body)
          ||
            begin
              capture_longest_match ();
              let recovered = interp_stmt (1::branch_addr) locals recover in
              assert recovered;
              false
            end
        | IL.Loop  (_, s, p)         ->
          (* Uninitialize any variables that were assigned in the loop. *)
          let rec uninit s = match s with
            | IL.Alt   (_, a, b)
            | IL.Block (_, a, b)
            | IL.Try   (_, a, b)   -> uninit a; uninit b
            | IL.Let   (_, idx, _) ->
              let cell = Scope.L.IdxMap.find idx locals in
              cell.value <- Uninitialized
            | _ -> () in
          let rec loop () =
            let passed = interp_stmt (0::branch_addr) locals s in
            let continues = passed && interp_pred locals p in
            uninit s;
            if continues then (ignore (loop ()); true) else passed in
          loop ()
        | IL.Let   (_, lhs, rhs)     ->
          let cell = Scope.L.IdxMap.find lhs locals in
          (
(* TODO: Change LR handling so that multiple assignments never happen, or
   introduce a Release instruction that drops variables back to their
   pre-assignment state, and use that to gate LR and repetition.
            if cell.value = Uninitialized then
*)
              cell.value <- interp_expr locals rhs
(*
            else
              failwith (
                sprintf "Reassigning %s"
                  (Stringer.s Label.stringer (Scope.L.label formals lhs))
              )
*)
          );
          true
        | IL.Call  (_, idx, actuals) ->
          call idx (List.map (interp_expr locals) actuals)
        | IL.Mut   (_, eff)          ->
          interp_effect locals eff;
          true
        | IL.Panic _                 -> raise PanicExn
      in
      end_stmt passed;
      passed
    and interp_pred locals p = match p with
      | IL.Nand  ls                      ->
        not (List.fold_left (fun b q -> b && interp_pred locals q) true ls)
      | IL.Is    (e, t)                  ->
        (match interp_eexpr locals e, t with
          | OutputBuffer _,         IL.OutputBuffer_t
          | DomainData   (E.Str _), IL.InputBuffer_t  _ -> true
          | _,                      IL.OutputBuffer_t
          | _,                      IL.InputBuffer_t  _ -> false
          | DomainData   d,         _                   -> (match d, t with
              | E.Bool   _,         IL.Bool_t
              | E.Int    _,         IL.Int_t
              | E.Nil,              IL.Null_t
              | E.Num    _,         IL.Float_t
              | E.Arr    _,         IL.Array_t
              | E.Rel    _,         IL.Relation_t
              | E.Str    _,         IL.InputBuffer_t  _ -> true
              | E.Bool   _,         _
              | E.Int    _,         _
              | E.Nil,              _
              | E.Num    _,         _
              | E.Arr    _,         _
              | E.Rel    _,         _
              | E.Str    _,         _                   -> false
          )
          | v,                      t                   ->
            failwith (
              sprintf
                "Cannot perform runtime type check with value %s and type %s"
                (Stringer.s value_stringer           v)
                (Stringer.s IL.ReprStringers.ex_t t)
            )
        )
      | IL.Lt (e, f)               ->
        (* If the left-hand side is a lookahead then we don't actually assume
           that the lookahead hint applies because we're still checking whether
           we have space to find the characters we expect. *)
        let e = match e with
          | IL.Lookahead (c, n, _) -> IL.Lookahead (c, n, None)
          | _ -> e in
        (match interp_iexpr locals e, interp_iexpr locals f with
          | InputCursor  left,               InputCursor  right              ->
            assert (same left.buffer right.buffer);
            Interpreter.Reference.compare compare left.pos right.pos < 0
          | Snapshot     left,               Snapshot     right              ->
            left < right
          | InputCursor  { pos; _ },         Snapshot     snap               ->
            pos.get () < snap
          | Snapshot     snap,               InputCursor  { pos; _ }         ->
            snap < pos.get ()
          | DomainData   (E.Int i),          DomainData   (E.Int j)          ->
            i < j
          | Snapshot     _,                  DomainData   (E.Int -1)         ->
            false
          | DomainData   (E.Int -1),         Snapshot     _                  ->
            true
          | v,                               w                               ->
            failwith (
              sprintf "Cannot compare %s == %s"
                (Stringer.s value_stringer v)
                (Stringer.s value_stringer w));
        )
      | IL.Empty e                       ->
        (match interp_iexpr locals e with
          | InputCursor  { pos; buffer; _ }         ->
            pos.get () >= String.length buffer
          | ArrCursor    (els, i)                   -> !i >= List.length els
          | RelCursor    (els, i)                   -> !i >= List.length els
          | EnumValue    (_, Some Var.Value.Many s) -> Var.Symbols.is_empty s
          | v                                       ->
            type_mismatch (`P p) v (IL.IData (
              IL.InputCursor_t CUK.Unicode
            ))
        )
      | IL.IsMatch e                     ->
        (match interp_iexpr locals e with
          | Match None       -> false
          | Match (Some _)   -> true
          | v                ->
            type_mismatch (`P p) v
              (IL.IData (IL.Match_t (IL.Anchored, CUK.Unicode)))
        )
      | IL.In (e, ints)                  ->
        (match interp_iexpr locals e with
          | EnumValue (d, None) ->
            failwith (
              Printf.sprintf "Enum variable in %s not set"
                (Stringer.s Var.Domain.stringer d)
            )
          | EnumValue (d, vo) -> (match Opt.map (Var.Domain.ordinal_i d) vo with
              | Some (Some i) -> IL.OpenRange.Set.has ints (IL.Point i)
              | _             -> failwith "invalid or uninitialized value")
          | CodeUnit  (cu, _) ->
            IL.OpenRange.Set.has ints (IL.Point (CodeUnit.as_int cu))
          | v                 ->
            type_mismatch (`P p) v (IL.IData (IL.CodeUnit_t CUK.Unicode))
        )
      | IL.BoolIdent e                   ->
        (match interp_iexpr locals e with
          | Bool      b      -> b
          | v                -> type_mismatch (`P p) v (IL.IData IL.IBool_t)
        )
    and interp_eexpr locals e = match e with
      | IL.ERef         idx    ->
        let cell = Scope.L.IdxMap.find idx locals in
        (match cell.value with
          | Uninitialized -> failwith "uninitialized"
          | x -> x)
      | IL.AllocBuffer  (s, e) ->
        let to_int e = match interp_iexpr locals e with
          | InputCursor  c -> c.pos.get ()
          | Snapshot     x -> x
          | v              ->
            type_mismatch (`IE e) v
              (IL.IData (IL.InputSnapshot_t CUK.Unicode)) in
        let start_offset = to_int s in
        let end_offset   = to_int e in
        let capacity_hint = end_offset - start_offset in
        OutputBuffer (Buffer.make ~size:capacity_hint ())
      | IL.FreezeBuffer (b, _) ->
        (match interp_eexpr locals b with
          | OutputBuffer b -> DomainData (E.Str (Buffer.to_string b))
          | v              ->
            type_mismatch (`EE b) v (IL.EData IL.OutputBuffer_t)
        )
      | IL.SliceBuffer  (b, s, e, _) ->
        (match interp_eexpr locals b with
          | OutputBuffer b      -> (match interp_iexpr locals s with
              | Snapshot si     -> (match interp_iexpr locals e with
                  | Snapshot ei ->
                    DomainData (E.Str (Buffer.sub b si ei))
                  | v           ->
                    type_mismatch (`IE e) v (IL.IData IL.OutputSnapshot_t))
              | v               ->
                type_mismatch (`IE s) v (IL.IData IL.OutputSnapshot_t))
          | v                   ->
            type_mismatch (`EE b) v (IL.EData IL.OutputBuffer_t)
        )
      | IL.StrLit    s                   -> DomainData (E.Str s)
      | IL.ElAt      c                   ->
        (match interp_iexpr locals c with
          | ArrCursor (ls, {contents=i}) -> DomainData (List.nth ls i)
          | v                            ->
            type_mismatch (`IE c) v (IL.IData IL.ArrCursor_t))
      | IL.KeyAt     c                   ->
        (match interp_iexpr locals c with
          | RelCursor (ls, {contents=i}) -> DomainData (fst (List.nth ls i))
          | v                            ->
            type_mismatch (`IE c) v (IL.IData IL.RelCursor_t))
      | IL.ValAt     c                   ->
        (match interp_iexpr locals c with
          | RelCursor (ls, {contents=i}) -> DomainData (snd (List.nth ls i))
          | v                            ->
            type_mismatch (`IE c) v (IL.IData IL.RelCursor_t))
      | IL.Itoa      e                   ->
        (match interp_eexpr locals e with
          | DomainData (E.Int i)         -> DomainData (E.Str (string_of_int i))
          | v                            ->
            type_mismatch (`EE e) v (IL.EData IL.Int_t))
      | IL.Ftoa      e                   ->
        (match interp_eexpr locals e with
          | DomainData (E.Int i)         ->
            DomainData (E.Str (Enc.encode_float (float_of_int i)))
          | DomainData (E.Num f)         ->
            DomainData (E.Str (Enc.encode_float f))
          | v                            ->
            type_mismatch (`EE e) v (IL.EData IL.Float_t))
      | IL.Cptoa     e                   ->
        (match interp_iexpr locals e with
          | CodeUnit (cu, _)             ->
            let cp = Unicode.i2uni (CodeUnit.as_int cu) in
            DomainData (E.Str (UnicodeSeq.to_utf8 (UnicodeSeq.singleton cp)))
          | v                            ->
            type_mismatch (`IE e) v
              (IL.IData (IL.CodeUnit_t CUK.Unicode)))
      | IL.Ntoa      (e, scv)            ->
        let n = (match interp_iexpr locals e with
          | DomainData (E.Int i)         -> i
          | CodeUnit   (cu, _)           -> CodeUnit.as_int cu
          | v                            ->
            type_mismatch (`IE e) v (IL.EData IL.Int_t)) in
        DomainData (E.Str (
          NumberSystem.encode_integer ~ns:scv.SCV.ns ~n
            ~min_digits:(SCV.min_digits scv.SCV.sequences)))
    and interp_iexpr locals expr = match expr with
      | IL.IRef  idx ->
        let cell = Scope.L.IdxMap.find idx locals in
        (match cell.value with
          | Uninitialized ->
            failwith (
              sprintf "Failed to read %s"
                (Label.to_string (Scope.L.label formals idx))
            )
          | x -> x)
      | IL.GRef  idx ->
        let cell = Scope.G.IdxMap.find idx global_environment in
        (match cell.value with
          | Uninitialized -> failwith "uninitialized"
          | x -> x)
      | IL.Deref e ->
        (match interp_iexpr locals e with
          | ShallowPtr p -> p.get ()
          | v            -> type_mismatch (`IE expr) v (IL.SPtr IL.IBool_t)
        )
      | IL.AllocPtr t -> (match t with
          | IL.Enum_t domain ->
            ShallowPtr (Interpreter.Reference.make (EnumValue (domain, None)))
          | IL.IBool_t ->
            ShallowPtr (Interpreter.Reference.make (Bool false))
          | IL.Match_t _ ->
            ShallowPtr (Interpreter.Reference.make (Match None))
          | _     -> failwith "non enum type used for pointer"
      )
      | IL.StartOf e ->
        (match interp_eexpr locals e, IL.typeof globals formals (`EE e) with
          | DomainData   (E.Str buffer), IL.EData (IL.InputBuffer_t cuk) ->
            let pos = Interpreter.Reference.make 0 in
            let cursor = InputCursor { buffer; pos; cuk } in
            if is_none !first_input_cursor then
              first_input_cursor := Some cursor;
            cursor
          | DomainData (E.Arr els),      IL.EData IL.Array_t             ->
            ArrCursor (els, ref 0)
          | DomainData (E.Rel els),      IL.EData IL.Relation_t          ->
            RelCursor (els, ref 0)
          | v,                           IL.EData IL.Array_t             ->
            type_mismatch (`EE e) v (IL.EData IL.Array_t)
          | v,                           IL.EData IL.Relation_t          ->
            type_mismatch (`EE e) v (IL.EData IL.Relation_t)
          | v,                           _                               ->
            type_mismatch (`EE e) v (IL.EData (IL.InputBuffer_t CUK.Unicode))
        )
      | IL.EndOf e ->
        (match interp_eexpr locals e with
          | DomainData    (E.Str s) -> Snapshot (String.length s)
          | DomainData    (E.Arr l) -> Snapshot (List.length l)
          | DomainData    (E.Rel l) -> Snapshot (List.length l)
          | OutputBuffer  buffer    -> Snapshot (Buffer.length buffer)
          | v                       ->
            type_mismatch (`IE expr) v (IL.EData (IL.InputBuffer_t CUK.Unicode))
        )
      | IL.Read e ->
        (match interp_iexpr locals e with
          | InputCursor  { buffer; pos; cuk; _ } ->
            let cu, _ = CUK.select cuk buffer (pos.get ()) in
            CodeUnit (cu, cuk)
          | v                                    ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.InputCursor_t CUK.Unicode))
        )
      | IL.Lookahead (e, n, h) ->
        let e_value = interp_iexpr locals e in
        let n_value = interp_iexpr locals n in
        let n = match n_value with
          | DomainData (Encodable.Int n) ->
            assert (n >= 0);
            n
          | _ -> type_mismatch (`IE n) n_value (IL.IData IL.IInt_t)
        in
        (match e_value with
          | InputCursor { buffer; pos; cuk } ->
            let rec lookahead pos' n =
              if n = 0 then
                pos'
              else if pos' = String.length buffer then
                max_int  (* Allow range checking using lookahead past end. *)
              else begin
                let cu, pos' = CUK.select cuk buffer pos' in
                assert (match h with
                  | Some cus_hint -> CodeUnit.Range.Set.has cus_hint cu
                  | None          -> true);
                lookahead pos' (n - 1)
              end in
            let pos_ahead = Interpreter.Reference.make (
              lookahead (pos.get ()) n
            ) in
            InputCursor { buffer; pos = pos_ahead; cuk }
          | ArrCursor   (els, i) -> ArrCursor (els, ref (!i + n))
          | RelCursor   (els, i) -> RelCursor (els, ref (!i + n))
          | v ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.InputCursor_t CUK.Unicode))
        )
      | IL.FindAt    (re, start, limit)
      | IL.FindFirst (re, start, limit) ->
        let apply = (match expr with
          | IL.FindAt _ -> Regex.apply_at
          | _           -> Regex.apply_after) in
        (match interp_iexpr locals start with
          | InputCursor ({ buffer; pos; cuk; _ }) ->
            let start_pos = pos.get () in
            (match interp_iexpr locals limit with
              | Snapshot limit_pos ->
                let select = CUK.select cuk in
                let chunk =
                  StrCursor.of_substr select buffer start_pos limit_pos in
                let match_result =
                  apply re Regex.str_cursor_reader ~is_eof:true [chunk] in
                (match match_result with
                  | Regex.Match.NoMatch           -> Match None
                  | Regex.Match.Complete regions ->
                    (match regions.Regex.Match.at with
                      | [region] ->
                        Match (Some (
                          (* TODO: none for anchored *)
                          Some (StrCursor.as_index region),
                          StrCursor.limit_as_index region
                        ))
                      | _ -> failwith
                        "single input split across multiple regions")
                  | Regex.Match.Prefix   _        -> failwith "bad apply"
                )
              | v ->
                type_mismatch (`IE limit) v
                  (IL.IData (IL.InputCursor_t CUK.Unicode))
            )
          | v ->
            type_mismatch (`IE start) v
              (IL.IData (IL.InputCursor_t CUK.Unicode))
        )
      | IL.StartOfMatch e ->
        (match interp_iexpr locals e with
          | Match (Some (Some start_cursor, _)) -> Snapshot start_cursor
          | Match (Some (None,              _)) -> failwith "anchored match"
          | Match None                          -> failwith "no match"
          | v                                   ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.Match_t (IL.Unanchored, CUK.Unicode)))
        )
      | IL.EndOfMatch   e ->
        (match interp_iexpr locals e with
          | Match (Some (_, end_cursor)) -> Snapshot end_cursor
          | Match None -> failwith "no match"
          | v                            ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.Match_t (IL.Anchored, CUK.Unicode)))
        )
      | IL.MakeMatch    (s, e) ->
        let reduce_to_snapshot e = match interp_iexpr locals e with
          | Snapshot    pos        -> pos
          | InputCursor { pos; _ } -> pos.get()
          | v ->
            type_mismatch (`IE e) v
              (IL.IData (IL.InputSnapshot_t CUK.Unicode)) in
        Match (Some (Opt.map reduce_to_snapshot s, reduce_to_snapshot e))
      | IL.Snapshot     e ->
        (match interp_iexpr locals e with
          | InputCursor  { pos; _ } -> Snapshot (pos.get ())
          | ArrCursor    (_, p_ref)
          | RelCursor    (_, p_ref) -> Snapshot !p_ref
          | v                       ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.InputCursor_t CUK.Unicode))
        )
      | IL.CopyCursor   (e, offset) ->
        let copy { buffer; pos; cuk } =
          let pos' = Interpreter.Reference.make (
            match offset with
              | None         -> pos.get ()
              | Some new_pos -> (match interp_iexpr locals new_pos with
                  | Snapshot snapshot_pos -> snapshot_pos
                  | v ->
                    type_mismatch (`IE new_pos) v
                      (IL.IData (IL.InputSnapshot_t CUK.Unicode))
              )
          ) in
          { buffer; pos = pos'; cuk } in
        (match interp_iexpr locals e with
          | InputCursor  cur     -> InputCursor  (copy cur)
          | ArrCursor    (ls, i) -> ArrCursor    (ls, ref !i)
          | RelCursor    (ls, i) -> RelCursor    (ls, ref !i)
          | v                    ->
            type_mismatch (`IE expr) v
              (IL.IData (IL.InputCursor_t CUK.Unicode))
        )
      | IL.EnumConst    (d, v) -> EnumValue (d, Some v)
      | IL.Bool         b      -> Bool      b
      | IL.IntLit       i      -> DomainData (E.Int i)
      | IL.ToPrim       (e, t) -> (match interp_eexpr locals e, t with
          | DomainData (E.Bool b),     IL.Bool_t -> Bool b
          | DomainData (E.Int _) as v, IL.Int_t  -> v
          | v,                         t         ->
            type_mismatch (`EE e) v (IL.EData t)
      )
      | IL.Atoi     (s, k, ns) -> (match interp_eexpr locals s with
          | DomainData    (E.Str s) ->
            debug (fun () -> sprintf "atoi(`%s`, %d)" s (ns.NumberSystem.base));
            let i = NumberSystem.decode_integer ns s 0 (String.length s) in
            if 0 <= i && i < CUK.n_units k then
              CodeUnit (CodeUnit.of_int i, k)
            else
              failwith (Printf.sprintf
                          "Decoded value %d not a valid code-unit of kind %s"
                          i (Stringer.s CUK.stringer k))
          | v                       ->
            type_mismatch (`EE s) v (IL.EData (IL.InputBuffer_t CUK.Unicode)))
      | IL.Succ         e      -> (match interp_iexpr locals e with
          | DomainData (E.Int i) -> DomainData (E.Int (i + 1))
          | v                  ->
            type_mismatch (`IE e) v (IL.IData IL.Counter_t))
      | IL.Nin          (d, c) ->
        let universe = Var.Domain.symbols d in
        let intersection = List.fold_left
          (fun ss e -> match interp_iexpr locals e with
            | EnumValue (de, Some v) ->
                (* Require that symbol to integer maps are identical except for
                   Many/One distinction *)
              assert (
                let de' = Var.Domain.(
                  match de with | One ls | Many ls -> Many ls
                ) in
                Var.Domain.equal d de'
              );
              let vs = match v with
                | Var.Value.One  s  -> Var.Symbols.singleton s
                | Var.Value.Many ss -> ss in
              Var.Symbols.inter ss vs
            | EnumValue (_, None)    ->
              failwith (
                Printf.sprintf "Unintiailized value from %s"
                  (Stringer.s IL.ReprStringers.iexpr e)
              )
            | v                      ->
              type_mismatch (`IE e) v (IL.IData (IL.Enum_t d))
          )
          universe
          c in
        EnumValue (
          d,
          Some (Var.Value.Many (Var.Symbols.diff universe intersection))
        )
    and interp_expr locals e = match e with
      | `IE ie -> interp_iexpr locals ie
      | `EE ee -> interp_eexpr locals ee
    and interp_effect locals eff = begin
      debug (fun () ->
        sprintf "Effect %s" (Stringer.s IL.ReprStringers.sideeff eff);
      );
      match eff with
        | IL.SetGlobal (lhs, rhs)           ->
          let cell = Scope.G.IdxMap.find lhs global_environment in
          cell.value <- interp_iexpr locals rhs
        | IL.SetPtr    (lhs, rhs)           ->
          let cell = Scope.L.IdxMap.find lhs locals in
          (match cell.value with
            | ShallowPtr storage -> storage.set (interp_iexpr locals rhs)
            | v                  ->
              let lbl = Scope.L.label formals lhs in
              failwith (
                sprintf
                  "Cannot set pointer %s with value %s and type %s"
                  (Stringer.s Label.stringer         lbl)
                  (Stringer.s value_stringer         v)
                  (Stringer.s IL.ReprStringers.ltype cell.typ)
              )
          )
        | IL.Append    (IL.Cptoa cp, dest)  ->
          let cu, cuk = (match interp_iexpr locals cp with
            | CodeUnit (c, k) -> c, k
            | v               ->
              type_mismatch (`IE cp) v
                (IL.IData (IL.CodeUnit_t CUK.Unicode))) in
          (match interp_eexpr locals (IL.ERef dest) with
            | OutputBuffer output_buffer ->
              (* When we append code-units one at a time, we need to be careful
                 to merge them properly so that we end up with complete
                 code-points in the output buffers native encoding. *)
              (match cuk with
                | CUK.Utf16 ->
                  (* Merge into any lead surrogate to form
                     a full code-point. *)
                  let cu', cuk' = begin
                    let cui = CodeUnit.as_int cu in
                    let buf_len = Buffer.length output_buffer in
                    let char_at = Buffer.char_at output_buffer in
                    if (0xDC00 <= cui && cui < 0xE000
                      (* A leading surrogate will always by UTF-8 encoded
                         using a 3-byte form as \xED\xA?\x?? *)
                        && buf_len >= 3
                        && chr_eq '\xED' (char_at (buf_len - 3))
                        && (((int_of_char (char_at (buf_len - 2)))
                             land 0xF0) = 0xA0)) then
                      (* Pull the 10 significant bits from the last 2 bytes. *)
                      let leading_surrogate_bits =
                        (
                          ((int_of_char (char_at (buf_len - 2)))
                           land 0x0f)
                          lsl 6
                        )
                        lor (
                          (int_of_char (char_at (buf_len - 1)))
                          land 0x3f
                        ) in
                      let cp_bits = 0x10000 lor
                        (leading_surrogate_bits lsl 10)
                        lor (cui land 0x3ff) in
                      Buffer.truncate output_buffer (buf_len - 3);
                      CodeUnit.of_int cp_bits, CUK.Unicode
                    else
                      cu, cuk
                  end in
                  CUK.emit cuk' cu' output_buffer
                | CUK.NullAlphabet
                | CUK.Octet
                | CUK.Unicode
                | CUK.OctetTriplet ->
                  CUK.emit cuk cu output_buffer
              )
            | v -> type_mismatch (`SE eff) v (IL.EData IL.OutputBuffer_t)
          )
        | IL.Append    (str, dest)          ->
          let s = (match interp_eexpr locals str with
            | DomainData (E.Str s) -> s
            | v                    ->
              type_mismatch (`EE str) v
                (IL.EData (IL.InputBuffer_t CUK.Unicode))) in
          debug (fun () -> sprintf "\tstr=%s" (Stringer.s Stringer.string s));
          (match interp_eexpr locals (IL.ERef dest) with
            | OutputBuffer output_buffer ->
              Buffer.append output_buffer s
            | v -> type_mismatch (`SE eff) v (IL.EData IL.OutputBuffer_t)
          )
        | IL.AppendMks (mks, dest)          ->
          debug (fun () ->
            String.concat " "
              (List.map (fun mk ->
                sprintf "\tMarks %s"
                  (match mk with
                    | EvMarker.StartUserOp (i, _) ->
                      Stringer.s op_stringer       (int_to_op i)
                    | _                           ->
                      Stringer.s EvMarker.stringer mk)
               ) mks)
          );
          List.iter (
            fun mk -> match interp_eexpr locals (IL.ERef dest) with
              | OutputBuffer output_buffer ->
                encode_marker_onto mk output_buffer
              | v -> type_mismatch (`SE eff) v (IL.EData IL.OutputBuffer_t)
          ) mks
        | IL.CopyTo    (start, limit, dest) ->
          (match (interp_iexpr locals start,
                  interp_iexpr locals limit,
                  interp_eexpr locals (IL.ERef dest)) with
            | (InputCursor  { buffer; pos; _ },
               Snapshot     limit_pos,
               OutputBuffer output_buffer) ->
              let start_pos = pos.get () in
              Buffer.append_sub output_buffer buffer start_pos limit_pos
            | InputCursor _, Snapshot _, v ->
              type_mismatch (`SE eff) v (IL.EData IL.OutputBuffer_t)
            | InputCursor _, v,             _ ->
              type_mismatch (`SE eff) v
                (IL.IData (IL.InputSnapshot_t CUK.Unicode))
            | v,             _,             _ ->
              type_mismatch (`SE eff) v
                (IL.IData (IL.InputCursor_t CUK.Unicode))
          )
        | IL.SetCursor (lhs, rhs)           ->
          let cell = Scope.L.IdxMap.find lhs locals in
          let new_pos = interp_iexpr locals rhs in
          debug (fun () ->
            sprintf "\tSetting cursor %s <- %s"
              (Stringer.s value_stringer cell.value)
              (Stringer.s value_stringer new_pos)
          );
          (match cell.value, new_pos with
            | InputCursor cursor, Snapshot    new_pos      ->
              mut_count := !mut_count + new_pos - cursor.pos.get ();
              cursor.pos.set new_pos
            | InputCursor cursor, InputCursor { pos; _ }   ->
              let new_pos = pos.get () in
              mut_count := !mut_count + new_pos - cursor.pos.get ();
              cursor.pos.set new_pos
            | ArrCursor   (_, r), Snapshot    new_pos
            | RelCursor   (_, r), Snapshot    new_pos      ->
              mut_count := !mut_count + new_pos - !r;
              r := new_pos
            | ArrCursor   (_, r), ArrCursor   (_, new_pos)
            | RelCursor   (_, r), RelCursor   (_, new_pos) ->
              mut_count := !mut_count + !new_pos - !r;
              r := !new_pos
            | InputCursor _,      v
            | ArrCursor   _,      v
            | RelCursor   _,      v                        ->
              type_mismatch (`SE eff) v
                (IL.IData (IL.InputSnapshot_t CUK.Unicode))
            | v,                  _                        ->
              type_mismatch (`SE eff) v
                (IL.IData (IL.InputCursor_t CUK.Unicode))
          )
        | IL.Truncate  (new_end, out)       ->
          (match interp_iexpr locals new_end with
            | Snapshot pos ->
              (match interp_eexpr locals (IL.ERef out) with
                | OutputBuffer buffer -> Buffer.truncate buffer pos;
                | v -> type_mismatch (`SE eff) v (IL.EData IL.OutputBuffer_t)
              )
            | v ->
              type_mismatch (`SE eff) v
                (IL.IData (IL.InputSnapshot_t CUK.Unicode))
          )
        | IL.Incr      (idx, n, h)          ->
          interp_effect locals
            (IL.SetCursor (idx, IL.Lookahead (IL.IRef idx, n, h)))
    end in

    let call_stack_frame, unused_actuals, unused_arity = Scope.L.fold
      (fun (csf, actuals, arity) actual_idx label typ ->
        let value, actuals', arity' = match actuals with
          | []               -> Uninitialized, [],       0
          | actual::actuals' -> actual,        actuals', arity - 1 in
        let csf' = Scope.L.IdxMap.add actual_idx { label; typ; value } csf in
        (csf', actuals', arity'))
      (Scope.L.IdxMap.empty, actuals, arity) formals in
    assert (is_empty unused_actuals);
    assert (unused_arity   = 0);
    interp_stmt [] call_stack_frame body
  end in

  let passed_opt, output_buffer, output_buffer_start = begin
    let formals = signature.Signature.formals in
    if List.length inputs <> List.length formals then begin
      failwith (
        Printf.sprintf "Signature mismatch %s vs %s"
          (Stringer.s (Stringer.list Actual.stringer) inputs)
          (Stringer.s (Stringer.list Formal.stringer) formals);
      )
    end;
    let output_buffer_opt = List.fold_left
      (fun o a -> match a with
        | Actual.OutputBuffer b -> Some b
        | _                     -> o)
      None inputs in
      (* Convert inputs to values. *)
    let actual_values = List.map2 actual_to_value formals inputs in
    List.iter
      (fun x -> match x with
        | InputCursor _ when is_none !first_input_cursor ->
          first_input_cursor := Some x
        | _ -> ())
      actual_values;

    let output_buffer = match output_buffer_opt with
      | Some b -> b
      | None   -> Buffer.make () in
    let output_buffer_start = Buffer.length output_buffer in

    let passed_opt =
      try
        Some (call start actual_values)
      with | PanicExn -> None
    in

    passed_opt, output_buffer, output_buffer_start
  end in

  let build_tree buffer =
    (* Convert the buffer to a tree of LR and Pushback nodes. *)
    let buf_len = String.length buffer in
    let rec build_pb_tree text_start pos children_rev =
      if pos = buf_len then
        List.rev (maybe_push_text text_start pos children_rev), pos
      else
        match decode_marker_from buffer pos with
          | None -> build_pb_tree text_start (pos+1) children_rev
          | Some (EvMarker.StartLR       as m, pos')
          | Some (EvMarker.StartPushback as m, pos') ->
            (* Consume any text before the start marker. *)
            let children_rev' = maybe_push_text text_start pos children_rev in
            (* Recurse to parse the nested content. *)
            let lr_children, pos' = build_pb_tree pos' pos' [] in
            (* Build the inner node *)
            let nested_region, end_marker = (match m with
              | EvMarker.StartLR       ->
                PbTree.LR lr_children, EvMarker.EndLR
              | EvMarker.StartPushback ->
                PbTree.Pushback lr_children, EvMarker.EndPushback
              | _ -> failwith "overmatch") in
            let children_rev' = nested_region::children_rev' in
            (* Consume the end marker *)
            let pos' = (match decode_marker_from buffer pos' with
              | Some (m, pos') when EvMarker.equal m end_marker -> pos'
              | _ -> failwith "end marker mismatch") in
            (* Recurse to parse right siblings *)
            build_pb_tree pos' pos' children_rev'
          | Some (EvMarker.StartUserOp  _ as m, pos')
          | Some (EvMarker.EndUserOp      as m, pos')
          | Some (EvMarker.CancelUserOp   as m, pos')
          | Some (EvMarker.PushEncoder  _ as m, pos')
          | Some (EvMarker.PopEncoder     as m, pos') ->
            let children_rev' =
              (PbTree.Marker m)
              ::(maybe_push_text text_start pos children_rev) in
            build_pb_tree pos' pos' children_rev'
          | Some (EvMarker.EndLR,       _)
          | Some (EvMarker.EndPushback, _) ->
            List.rev (maybe_push_text text_start pos children_rev), pos
    and maybe_push_text left right children_rev =
      if left = right then
        children_rev
      else
        (
          PbTree.Text (String.sub buffer left (right - left))
        )::children_rev in
    let pb_tree =
      let pb_tree, pos =
        build_pb_tree output_buffer_start output_buffer_start [] in
      if pos = buf_len then
        pb_tree
      else
        failwith "unused markers" in
    (* Use the side tables to map encoder indices to labels. *)
    let encoder_label_for_index encoder_index =
      let rec lookup side_tables = match side_tables with
        | (SideTable.Encoders table)::_  -> List.nth table encoder_index
        | _                         ::tl -> lookup tl
        | []                             -> failwith "missing encoder table" in
      lookup side_tables in
    (* Interpret pushback nodes, so that the start and end of user operations
       nest properly. *)
    let rec pushback ls pre_rev post_rev = match ls with
      | [] -> List.fold_left (@) (List.rev post_rev) pre_rev
      | PbTree.Pushback pbs::tl ->
        pushback tl (pbs::pre_rev) post_rev
      | PbTree.LR       lrs::tl ->
        pushback tl pre_rev
          (List.rev_append (pushback lrs [] []) post_rev)
      | hd                 ::tl ->
        pushback tl pre_rev (hd::post_rev) in
    let pb_tree' = pushback pb_tree [] [] in
    (* Walk levels of the tree to match StartUserOp with {Cancel,End}UserOp, and
       PushEncoding with PopEncoding. *)
    let rec build_op_tree ls =
      let decode_failed msg =
        let rec enumerate_ops i ls =
          (* Stop at an arbitrary but usually large enough pt. *)
          if i = 0x80 then
            let rec trim_nil ls = match ls with
              | E.Nil::tl -> trim_nil tl
              | _         -> ls in
            E.Arr (List.rev (trim_nil ls))
          else
            let el = try Some (int_to_op i) with | Not_found -> None in
            let ls' = (
              match el with
                | Some op -> E.Str (Stringer.s op_stringer op)
                | None    -> E.Nil
            )::ls in
            enumerate_ops (i+1) ls' in
        failwith (
          sprintf
            ("%s\n\n"
             ^^ "pools\n%s\n\n"
             ^^ "int_to_op\n%s\n\n"
             ^^ "output_buffer\n%s\n\n"
             ^^ "tree\n%s\n\n"
             ^^ "unprocessed\n%s")
            msg
            (Stringer.s (Stringer.list SideTable.stringer) side_tables)
            (Stringer.s E.json_stringer (enumerate_ops 0 []))
            (Stringer.s Stringer.string (Buffer.to_string output_buffer))
            (Stringer.s (Stringer.list PbTree.stringer) pb_tree')
            (Stringer.s (Stringer.list PbTree.stringer) ls)
        ) in
      match ls with
        | []     -> [], []
        | hd::tl ->
          (match hd with
            | PbTree.LR       _
            | PbTree.Pushback _
            | PbTree.Marker   (EvMarker.StartLR)
            | PbTree.Marker   (EvMarker.EndLR)
            | PbTree.Marker   (EvMarker.StartPushback)
            | PbTree.Marker   (EvMarker.EndPushback)       ->
              decode_failed "pushback failed"
            | PbTree.Marker   (EvMarker.EndUserOp)
            | PbTree.Marker   (EvMarker.CancelUserOp)
            | PbTree.Marker   (EvMarker.PopEncoder)        ->
              [], ls
            | PbTree.Marker   (EvMarker.StartUserOp (n,_)) ->
              let children, unprocessed = build_op_tree tl in
              (match unprocessed with
                | PbTree.Marker (EvMarker.EndUserOp)   ::siblings ->
                  let sibling_trees, unprocessed = build_op_tree siblings in
                  let op = OpTree.Op (int_to_op n, children) in
                  (op::sibling_trees),        unprocessed
                | PbTree.Marker (EvMarker.CancelUserOp)::siblings ->
                  let sibling_trees, unprocessed = build_op_tree siblings in
                  (children @ sibling_trees), unprocessed
                | [] -> decode_failed "missing end marker"
                | _  -> decode_failed "expected end marker"
              )
            | PbTree.Marker   (EvMarker.PushEncoder ei)    ->
              let children, unprocessed = build_op_tree tl in
              (match unprocessed with
                | PbTree.Marker (EvMarker.PopEncoder)::siblings ->
                  let sibling_trees, unprocessed = build_op_tree siblings in
                  let enc_label = encoder_label_for_index ei in
                  let embed = OpTree.Embed (enc_label, children) in
                  (embed::sibling_trees), unprocessed
                | [] -> decode_failed "missing end marker"
                | _  -> decode_failed "expected end marker"
              )
            | PbTree.Text     text                         ->
              let siblings, unprocessed = build_op_tree tl in
              ((OpTree.Leaf text)::siblings), unprocessed
          ) in
    let op_tree, unprocessed = build_op_tree pb_tree' in
    debug Stringer.(fun () ->
      sprintf "inputs=%s\nbuffer=%s\npb_tree=%s\nop_tree=%s\nunprocessed=%s"
        (s (list Actual.stringer)                    inputs)
        (s string                                    buffer)
        (s (list PbTree.stringer)                    pb_tree)
        (s (list (OpTree.node_stringer op_stringer)) op_tree)
        (s (list PbTree.stringer)                    unprocessed)
    );
    if is_empty unprocessed then
      op_tree
    else
      failwith "orphaned end marker" in

  debug (fun _ ->
    sprintf "passed_opt=%s"
      (Stringer.s (Stringer.option Stringer.bool) passed_opt));

  match passed_opt with
    | None ->
      PegResult.Panic
    | Some true ->
      let tree = build_tree (Buffer.to_string output_buffer) in
      Buffer.truncate output_buffer output_buffer_start;

      let interpreter t_label t_inputs = begin
        if Label.Map.mem t_label programs then
          let signature, _, _, { CodeUnitKinds.data_kind; _ } =
            Label.Map.find t_label programs in
          match signature.Signature.kind with
            | `Dec ->
              let r = interpret ~op_stringer:DecoderOperator.stringer ~debugger
                DecoderILBridge.int_to_op (decode_op_tree_handler data_kind)
                programs t_label t_inputs in
              debug (fun _ ->
                sprintf "`Dec interp result=%s"
                  (Stringer.s
                     (PegResult.stringer
                        (PegResult.stringer Encodable.stringer))
                     r)
              );
              (match r with
                | PegResult.Parsed    x      -> x
                | PegResult.Panic            -> PegResult.Panic
                | PegResult.Malformed (s, i) -> PegResult.Malformed (s, i))
            | `Enc -> (match t_inputs with
                | [Actual.DomainData _ as input] ->
                  let enc_out = Buffer.make () in
                  let post_process _ _ op_tree = match op_tree with
                    | []              -> E.Str ""
                    | [OpTree.Leaf s] -> E.Str s
                    | _               ->
                      failwith (
                        Printf.sprintf "markers in encoder output: %s"
                          (Stringer.s (OpTree.stringer Stringer.ignore) op_tree)
                      ) in
                  interpret ~debugger (fun _ _ -> raise Not_found)
                    post_process programs t_label
                    [Actual.OutputBuffer enc_out; input]
                | _ ->
                  invalid_arg (
                    Stringer.s (Stringer.list Actual.stringer)
                      t_inputs
                  )
            )
            | `San ->
              let op_tree_handler = SanitizerILBridge.sanitize in
              let r = interpret ~op_stringer:Sanitizer.Operator.stringer
                ~debugger SanitizerILBridge.int_to_op op_tree_handler
                programs t_label t_inputs in
              PegResult.(match r with
                | Parsed    (Parsed    s)      -> Parsed (E.Str s)
                | Parsed    (Malformed (s, i))
                | Malformed (s, i)             -> Malformed (s, i)
                | Parsed    Panic
                | Panic                        -> Panic
              )
        else
          failwith (
            Printf.sprintf "Missing program %s %s"
              (Stringer.s Signature.stringer signature) (Label.to_string t_label)
          )
      end in
      PegResult.Parsed (
        interp_tree interpreter (Some output_buffer) tree
      )
    | Some false ->
      capture_longest_match ();
      Buffer.truncate output_buffer output_buffer_start;
      let input_string = List.fold_left
        (fun input_string input -> match input with
          | Actual.InputBuffer str
          | Actual.InputCursor (str, _)
          | Actual.DomainData  (E.Str str) -> str
          | _                              -> input_string)
        "" inputs in
      let pos = (match !longest_match with
        | Some pos when pos <= String.length input_string ->
          debug (fun () ->
            sprintf "longest_match=%d, matched=%s, unmatched=%s"
              pos
              (String.sub input_string 0 pos)
              (String.sub input_string pos (String.length input_string - pos))
          );
          pos
        | _                                       -> 0
      ) in
      PegResult.Malformed (input_string, pos)
