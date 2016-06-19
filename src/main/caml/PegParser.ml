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

module Match = Regex.Match

type id = int

let start_id = 0

module Id = struct
  type t = id

  let compare a b = compare a b
  let equal a b = a = b
  let hash a = a
  let stringer = Stringer.int

  let make_counter () =
    let i = ref start_id in
    let make_id () =
      let next = !i + 1 in
      if next = start_id then failwith "id overflow";
      i := next;
      next in
    make_id
end

module IdMap = MapUtil.Make (Id)
module IdSet = SetUtil.Make (Id)

module State = struct

  type ('meta, 'operator) t =
    | Token         of 'meta Regex.t
    | Concatenation of 'meta * (('meta, 'operator) t list)
    | Union         of 'meta * (('meta, 'operator) t list)
    | Repetition    of 'meta * ('meta, 'operator) t
    | Operation     of 'meta * 'operator * ('meta, 'operator) t * Var.Pred.t
    | Call          of 'meta * Id.t
    | VarDecl       of 'meta * Var.Name.t * ('meta, 'operator) t
    | VarAssign     of 'meta * Var.Name.t * Var.Expr.t * 'meta Var.Domain.t
    | VarTest       of 'meta * Var.Pred.t
    | MatchUntil    of 'meta * 'meta Regex.t * ('meta, 'operator) t
    | Embed         of 'meta * ('meta, 'operator) embed_envelope
                     * ('meta, 'operator) t * CodeUnitKinds.t
    | Extern        of 'meta * Identifier.t * Rw.t Var.Map.t * Var.Pred.t
                     * ('meta, 'operator) t
    | Panic         of 'meta
  and ('meta, 'operator) machine = {
    meta : 'meta;
    name : Identifier.t;
    body : ('meta, 'operator) t;
  }
  and ('meta, 'operator) machines = ('meta, 'operator) machine IdMap.t
  and ('meta, 'operator) embed_envelope = {
    pred    : Var.Pred.t;
    extent  : ('meta, 'operator) t;
    noembed : ('meta, 'operator) t;
    dec     : 'meta decoder_handle;
    enc     : 'meta EncoderHandle.t;
  }
  and 'meta decoder_handle = (
    'meta
    * ('meta, 'meta DecoderOperator.t) machines
    * CodeUnitKinds.t
  ) Handle.t

  let ctor_name_stringer out x = match x with
    | Token         _ -> out "Token"
    | Concatenation _ -> out "Cat"
    | Union         _ -> out "Union"
    | Repetition    _ -> out "Rep"
    | Operation     _ -> out "Op"
    | Call          _ -> out "Call"
    | VarDecl       _ -> out "Decl"
    | VarAssign     _ -> out "Assign"
    | VarTest       _ -> out "Test"
    | MatchUntil    _ -> out "MatchUntil"
    | Embed         _ -> out "Embed"
    | Extern        _ -> out "Extern"
    | Panic         _ -> out "Panic"

  let noop_meta_stringer _ _ = ()

  let rec stringer
      : 'o .
        ?meta_stringer:'m Stringer.t -> ('o Stringer.t) -> ('m, 'o) t Stringer.t
  = fun ?(meta_stringer=noop_meta_stringer) op_stringer out x ->
    let child_stringer = stringer ~meta_stringer:meta_stringer op_stringer in
    match x with
      | Token         r ->
        Stringer.ctor "Token" (Stringer.tup2 meta_stringer Regex.stringer) out
          (Regex.meta r, r)
      | Repetition    (m, n) ->
        Stringer.ctor "Repetition"
          (Stringer.tup2 meta_stringer child_stringer) out (m, n)
      | Concatenation (m, ls) ->
        Stringer.ctor "Concatenation"
          (Stringer.tup2 meta_stringer
             (Stringer.abbrev (Stringer.list child_stringer)))
          out (m, ls)
      | Union         (m, ls) ->
        Stringer.ctor "Union"
          (Stringer.tup2 meta_stringer
             (Stringer.abbrev (Stringer.list child_stringer))) out (m, ls)
      | Call          (m, id) ->
        Stringer.ctor "Call" (Stringer.tup2 meta_stringer Id.stringer) out
          (m, id)
      | Operation     (m, o, b, p) ->
        if Var.Pred.equal p Var.Pred._true then
          Stringer.ctor "Operation"
            (Stringer.tup3 meta_stringer op_stringer child_stringer)
            out (m, o, b)
        else
          Stringer.ctor "Operation"
            (Stringer.tup4 meta_stringer op_stringer child_stringer
               Var.Pred.stringer)
            out (m, o, b, p)
      | VarDecl       (m, n, c) ->
        Stringer.ctor "VarDecl"
          (Stringer.tup3 meta_stringer Var.Name.stringer child_stringer)
          out (m, n, c)
      | VarAssign     (m, n, e, d) ->
        Stringer.ctor "VarAssign"
          (Stringer.tup4 meta_stringer Var.Name.stringer Var.Expr.stringer
             Var.Domain.stringer)
          out (m, n, e, d)
      | VarTest       (m, p) ->
        Stringer.ctor "VarTest" (Stringer.tup2 meta_stringer Var.Pred.stringer)
          out (m, p)
      | MatchUntil    (m, r, n) ->
        Stringer.ctor "MatchUntil"
          (Stringer.tup3 meta_stringer Regex.stringer child_stringer)
          out (m, r, n)
      | Embed         (m, o, i, cuks) ->
        Stringer.ctor "Embed"
          (Stringer.tup4 meta_stringer
             (embed_envelope_stringer op_stringer) child_stringer
             CodeUnitKinds.stringer)
          out (m, o, i, cuks)
      | Extern        (m, nm, vs, p, b) ->
        Stringer.ctor "Extern"
          (Stringer.tup5 meta_stringer Identifier.stringer
             (Var.Map.stringer Rw.stringer) Var.Pred.stringer
             child_stringer)
          out (m, nm, vs, p, b)
      | Panic         m -> Stringer.ctor "Panic" meta_stringer out m

  and machine_stringer
      : 'o
      . ?meta_stringer:'m Stringer.t
        -> 'o Stringer.t
        -> ('m, 'o) machine Stringer.t
  = fun ?(meta_stringer=noop_meta_stringer) op_stringer out { name; body; _ } ->
    Stringer.rec2
      "name" Identifier.stringer
      "body" (stringer ~meta_stringer:meta_stringer op_stringer)
      out
      (name, body)
  and machines_stringer
      : 'o . ?meta_stringer:'m Stringer.t
        -> bool -> 'o Stringer.t -> ('m, 'o) machines Stringer.t
  = fun ?(meta_stringer=noop_meta_stringer) transparent op_stringer out
        machines ->
    let single =
      if transparent && 1 = IdMap.cardinal machines then
        match IdMap.min_binding machines with
          | key, start_machine when key = start_id -> Some start_machine.body
          | _                                      -> None
      else
        None in
    match single with
      | Some body -> stringer op_stringer out body
      | None      ->
        let machine_stringer =
          machine_stringer ~meta_stringer:meta_stringer op_stringer in
        Stringer.list (Stringer.tup2 Id.stringer machine_stringer)
          out (IdMap.bindings machines)
  and embed_envelope_stringer
      : 'o . 'o Stringer.t -> ('m, 'o) embed_envelope Stringer.t
  = fun op_stringer out { pred; extent; noembed; dec; enc } ->
    Stringer.rec5
      "pred"    Var.Pred.stringer
      "extent"  Stringer.ignore
      "noembed" (stringer op_stringer)
      "dec"     decoder_handle_stringer
      "enc"     EncoderHandle.stringer
      out
      (pred, extent, noembed, dec, enc)
  and decoder_handle_stringer out x =
    let hstr = Handle.stringer
      ((fun out (_, machines, cuks) -> Stringer.tup2
        (machines_stringer true DecoderOperator.stringer)
        CodeUnitKinds.stringer
        out (machines, cuks))
      ) in
    hstr out x

  let machines_stringer
      ?(meta_stringer=noop_meta_stringer) op_stringer machines =
    machines_stringer ~meta_stringer:meta_stringer false op_stringer machines

  let id_to_numeric_ident =
    let fake_ident_namespace = Identifier.Namespace.make "id" in
    fun id ->
      Identifier.make fake_ident_namespace ("_" ^ (Stringer.s Id.stringer id))

  let default_id_to_name = id_to_numeric_ident

  let repr_stringer
      ?(meta_stringer=noop_meta_stringer) ?(id_to_name=default_id_to_name)
      op_stringer out x =
    let comment m out = meta_stringer out m in
    let rec equiv_grammar
        (* Don't let outer Decoder grammar in Embed cause type over-binding. *)
        : 'op
        . 'op Stringer.t -> ('m, 'op) t
          -> (Stringer.sink -> unit) Grammar.grammar_body
    = fun op_stringer x ->
      let of_child x = equiv_grammar op_stringer x in
      match x with
        | Token         re -> equiv_grammar_re re
        | Repetition    (m, b) -> Grammar.Repetition (comment m, of_child b)
        | Operation     (m, o, b, p) ->
          let render_op out =
            let meta_toks_rev = ref [] in
            meta_stringer (fun s -> meta_toks_rev := (s::!meta_toks_rev)) m;
            if not (is_empty !meta_toks_rev) then begin
              List.iter out (List.rev !meta_toks_rev);
              out ",";
            end;
            op_stringer out o;
            if not (Var.Pred.equal p Var.Pred._true) then begin
              out ":";
              Var.Pred.stringer out p
            end in
          Grammar.Concatenation (render_op, [of_child b])
        | Concatenation (m, ls) ->
          Grammar.Concatenation (comment m, List.map of_child ls)
        | Union         (m, ls) ->
          Grammar.Union (comment m, Grammar.Ordering.Ordered,
                         List.map of_child ls)
        | Call          (m, id) ->
          Grammar.Reference (comment m, id_to_name id)
        | VarDecl       (m, n, c) ->
          Grammar.Annotation (comment m,
                              Grammar.Scope (n, Grammar.Recursivity.Flat),
                              of_child c)
        | VarAssign     (m, n, v, _) ->
          Grammar.Annotation (comment m, Grammar.Set (n, v),
                              Grammar.Concatenation (ignore, []))
        | VarTest       (m, p) ->
          Grammar.Annotation (comment m, Grammar.If p,
                              Grammar.Concatenation (ignore, []))
        | MatchUntil    (m, r, n) ->
          Grammar.Annotation (
            comment m, Grammar.Until (equiv_grammar_re r), of_child n)
        | Embed         (m, { dec; pred; _ }, i, _) ->
          let (_, dec_machines, _) = Handle.require dec in
          let dec_body = (IdMap.find start_id dec_machines).body in
          Grammar.Annotation (
            comment m,
            Grammar.Embedded (of_child i, pred),
            equiv_grammar DecoderOperator.stringer dec_body)
        | Extern        (m, nm, vs, p, b) ->
          Grammar.Annotation (
            comment m,
            Grammar.Entrust (nm, Var.Names.of_list (Var.Map.keys vs), p),
            of_child b
          )
        | Panic         m -> Grammar.Panic (comment m)
    and equiv_grammar_re re = match re with
      | Regex.CharSet (m, ranges) ->
        let cu2u cu = Unicode.i2uni (CodeUnit.as_int cu) in
        let ur = Unicode.Range.Set.make (CodeUnit.Range.Set.map
          (fun lt rt -> Unicode.Range.make (cu2u lt) (cu2u rt)) ranges) in
        Grammar.CharSet (comment m, ur)
      | Regex.Concatenation (m, ls) ->
        Grammar.Concatenation (comment m, List.map equiv_grammar_re ls)
      | Regex.Union (m, ls) ->
        Grammar.Union (comment m, Grammar.Ordering.Ordered,
                       List.map equiv_grammar_re ls)
      | Regex.Repetition (m, b) ->
        Grammar.Repetition (comment m, equiv_grammar_re b)
      | Regex.NegLookahead (m, b) ->
        let synth_id = Identifier.make Identifier.Namespace.synthetic in
        let var_name = Var.Name.make (synth_id "NEG_LA") in
        let sym_fail = VarsWellKnown.sym_fail in
        let sym_pass = VarsWellKnown.sym_pass in
        let exp_fail = Var.Expr.Val (Var.Value.One sym_fail) in
        let exp_pass = Var.Expr.Val (Var.Value.One sym_pass) in
        let passed = Var.Pred.Any (var_name, Var.Symbols.singleton sym_pass) in
        Grammar.Annotation (comment m,
                            Grammar.Scope (var_name, Grammar.Recursivity.Flat),
          Grammar.Concatenation (ignore, [
            Grammar.Union (ignore, Grammar.Ordering.Ordered, [
              Grammar.Annotation (ignore, Grammar.Set (var_name, exp_fail),
                                  equiv_grammar_re b);
              Grammar.Annotation (ignore, Grammar.Set (var_name, exp_pass),
                Grammar.Concatenation (ignore, []));
            ]);
            Grammar.Annotation (ignore, Grammar.If passed,
              Grammar.Concatenation (ignore, []));
          ])) in
    GrammarParser.make_body_stringer
      ~str_meta:(fun out f -> f out)
      out (equiv_grammar op_stringer x)

  let fold_children f x n = match n with
    | Concatenation (_, children)
    | Union         (_, children)       -> List.fold_left f x children
    | Repetition    (_, child)
    | Operation     (_, _, child, _)
    | VarDecl       (_, _, child)
    | MatchUntil    (_, _, child)
    | Extern        (_, _, _, _, child) -> f x child
    | Embed         (_, e, inner, _)    -> f (f (f x e.noembed) e.extent) inner
    | Token     _ | Call    _ | Panic _
    | VarAssign _ | VarTest _           -> x

  let iter_children f n = fold_children (fun () x -> f x) () n

  let unfold_children n ls = match n, ls with
    | Concatenation (m, _),        ls  -> Concatenation (m, ls)
    | Union         (m, _),        ls  -> Union         (m, ls)
    | Repetition    (m, _),        [b] -> Repetition    (m, b)
    | Operation     (m, op, _, p), [b] -> Operation     (m, op, b, p)
    | VarDecl       (m, v, _),     [b] -> VarDecl       (m, v, b)
    | MatchUntil    (m, limit, _), [b] -> MatchUntil    (m, limit, b)
    | Token         (r),           []  -> Token         (r)
    | Call          (m, callee),   []  -> Call          (m, callee)
    | VarAssign     (m, n, v, d),  []  -> VarAssign     (m, n, v, d)
    | VarTest       (m, p),        []  -> VarTest       (m, p)
    | Embed         (m, e, _, k),
      [noembed; extent; inner]         ->
      Embed (m, { e with noembed = noembed; extent = extent }, inner, k)
    | Extern        (m, n, f, p,_),[b] -> Extern        (m, n, f, p, b)
    | Panic         m,             []  -> Panic         m
    | Repetition    _,             _
    | Operation     _,             _
    | VarDecl       _,             _
    | MatchUntil    _,             _
    | Token         _,             _
    | Call          _,             _
    | VarAssign     _,             _
    | VarTest       _,             _
    | Embed         _,             _
    | Extern        _,             _
    | Panic         _,             _   -> failwith "wrong arity"

  let rec map_meta
      (* Work around monomorphic recursion assumptions by explicitly listing
         the type parameters that can vary so that omm does not lead to
         over-binding during type inference. *)
      : 'm_op 'n_op
      . ('m -> 'n) -> ('m_op -> 'n_op) -> ('m, 'm_op) t -> ('n, 'n_op) t
  = fun meta_f op_f n ->
    let mm s = map_meta meta_f op_f s in
    let rmm r = Regex.map_meta meta_f r in
    let omm { pred; noembed; extent; dec; enc } = {
      pred   = pred;
      noembed = mm noembed;
      extent  = mm extent;
      dec     = Handle.map
        (fun (m, machines, cuk) ->
          meta_f m,
          IdMap.map (machine_map_meta meta_f (DecoderOperator.map_meta meta_f))
            machines,
          cuk
        )
        dec;
      enc     = EncoderHandle.map meta_f enc;
    } in
    let dmm d = Var.Domain.map_meta meta_f d in
    match n with
      | Token         (r)          -> Token         (rmm r)
      | Repetition    (m, c)       -> Repetition    (meta_f m, mm c)
      | Concatenation (m, ls)      -> Concatenation (meta_f m, List.map mm ls)
      | Union         (m, ls)      -> Union         (meta_f m, List.map mm ls)
      | Operation     (m, o, c, p) -> Operation     (meta_f m, op_f o, mm c, p)
      | Call          (m, id)      -> Call          (meta_f m, id)
      | VarDecl       (m, n, c)    -> VarDecl       (meta_f m, n, mm c)
      | VarAssign     (m, n, v, d) -> VarAssign     (meta_f m, n, v, dmm d)
      | VarTest       (m, v)       -> VarTest       (meta_f m, v)
      | MatchUntil    (m, r, c)    -> MatchUntil    (meta_f m, rmm r, mm c)
      | Embed         (m, o, i, k) -> Embed         (meta_f m, omm o, mm i, k)
      | Extern        (m,n,f,p, b) -> Extern        (meta_f m, n, f, p, mm b)
      | Panic         m            -> Panic         (meta_f m)
  and machine_map_meta
      : 'm_op 'n_op
      . ('m -> 'n) -> ('m_op -> 'n_op) -> ('m, 'm_op) machine
        -> ('n, 'n_op) machine
  = fun meta_f op_f { meta; name; body } -> {
      meta = meta_f meta;
      name = name;
      body = map_meta meta_f op_f body;
    }
  and map_deep
      : 'm_op
      .  (('m, 'm_op) t -> ('m, 'm_op) t)
      -> ('m -> 'n) -> ('m_op -> 'n_op)
      -> (('n, 'n_op) t -> ('n, 'n_op) t)
      -> ('m, 'm_op) t
      -> ('n, 'n_op) t
  = fun pre map_meta map_op post n ->
    let mm = map_meta in
    let mo = map_op in
    let rec mn n =
      let n' = match pre n with
        | Token         (r)          -> Token         (mr r)
        | Concatenation (m, ls)      -> Concatenation (mm m, List.map mn ls)
        | Union         (m, ls)      -> Union         (mm m, List.map mn ls)
        | Repetition    (m, b)       -> Repetition    (mm m, mn b)
        | VarDecl       (m, v, b)    -> VarDecl       (mm m, v, mn b)
        | MatchUntil    (m, r, b)    -> MatchUntil    (mm m, mr r, mn b)
        | Call          (m, c)       -> Call          (mm m, c)
        | VarAssign     (m, n, e, d) -> VarAssign     (mm m, n, e, md d)
        | VarTest       (m, p)       -> VarTest       (mm m, p)
        | Operation     (m, o, b, p) -> Operation     (mm m, mo o, mn b, p)
        | Embed         (m, e, b, k) -> Embed         (mm m, me e, mn b, k)
        | Extern        (m,n,f,p, b) -> Extern        (mm m, n, f, p, mn b)
        | Panic         m            -> Panic         (mm m)
      in
      post n'
    and me { noembed; extent; enc; dec; pred } =
      let dec' = Handle.map
        (fun (m, machines, cuk) ->
          mm m,
          IdMap.map (machine_map_meta mm (DecoderOperator.map_meta mm))
            machines,
          cuk
        )
        dec in
      {
        noembed = mn noembed;
        extent  = mn extent;
        enc     = EncoderHandle.map mm enc;
        dec     = dec';
        pred
      }
    and mr re = Regex.map_meta mm re
    and md d = Var.Domain.map_meta mm d in
    mn n

  let meta s = match s with
    | Token         (r)             -> Regex.meta r
    | Repetition    (m, _)
    | Concatenation (m, _)
    | Union         (m, _)
    | Operation     (m, _, _, _)
    | Call          (m, _)
    | VarDecl       (m, _, _)
    | VarAssign     (m, _, _, _)
    | VarTest       (m, _)
    | MatchUntil    (m, _, _)
    | Embed         (m, _, _, _)
    | Extern        (m, _, _, _, _)
    | Panic         m               -> m

end
