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

module GrammarVariantSetMap = MapUtil.Make (GrammarVariant.Set)

module Make (R : Grammar.Reporting) = struct

  module G = Grammar

  module GMap = MapUtil.Make (struct
    type t = R.meta_t G.grammar_body * GrammarVariant.Set.t
    let compare (a, c) (b, d) = Cmp.chain (G.Compare.body a b)
      (lazy (GrammarVariant.Set.compare c d))
    let stringer out x =
      Stringer.tup2 GrammarParser.body_stringer GrammarVariant.Set.stringer
        out x
  end)

  module CMap = GMap
  module DMap = GMap
  module EMap = GMap
  module SMap = GMap

  module RSimplifier       = Simplifier.Make        (R)

  module DecoderCompiler   = DecoderCompiler.Make   (R)
  module EncoderCompiler   = EncToIL.Make           (R)
  module SanitizerCompiler = SanitizerCompiler.Make (R)
  module ContexterCompiler = ContexterCompiler.Make (R)

  type toolbox = {
    mutable decs     : R.meta_t DecoderHandle.t       DMap.t;
    mutable encs     : R.meta_t EncoderHandle.t       EMap.t;
    mutable sans     : R.meta_t SanitizerHandle.t     SMap.t;
    mutable cons     : R.meta_t ContexterHandle.t     CMap.t;
    mutable labels   : R.meta_t ToolUnion.t option    Label.Map.t;
    mutable variants : R.meta_t Grammar.grammar       GrammarVariantSetMap.t;
  }

  class linker
    (g        : R.meta_t G.grammar)
    (tb       : toolbox)
    (variants : GrammarVariant.Set.t) =
  object (self : R.meta_t #Linker.t)

    method variant v = begin
      if GrammarVariant.Set.subset v variants then
        (self :> linker)
      else begin
        let variants' = GrammarVariant.Set.union v variants in
        (* Assumes that GrammarVariant.derive is order-agnostic *)
        let g' = match GrammarVariantSetMap.find_opt variants' tb.variants with
          | Some g' -> g'
          | None    ->
            let g' = GrammarVariant.Set.fold
              (fun v g' -> match GrammarVariant.derive v (G.G g') with
                | G.G g'' -> g''
                | _      -> failwith "not grammar")
              (GrammarVariant.Set.diff v variants) g
            in
            let opts = {
              Simplifier.Opts.inline_factor = Simplifier.Opts.InlinePassFail
            } in
            let g', _ = RSimplifier.simplify ~opts g' [] in
            tb.variants <- GrammarVariantSetMap.add variants' g' tb.variants;
            g' in
        new linker g' tb variants'
      end
    end

    method private unique_label label =
      let rec uniquify n =
        let label', n' = match n with
            | None   -> label, Some 1
            | Some i -> (
              Label.of_string
                (Printf.sprintf "%s_%d" (Label.to_string label) i),
              Some (i+1)
            ) in
        if Label.Map.mem label' tb.labels then
          uniquify n'
        else
          label' in
      let uniq_label = uniquify None in
      tb.labels <- Label.Map.add_no_override uniq_label None tb.labels;
      (
        uniq_label,
        fun h ->
          assert (Label.equal uniq_label (ToolUnion.label h));
          tb.labels <- Label.Map.add uniq_label (Some h) tb.labels
      )
    (** [let uniq_label, store = self#unique_label lbl] assigns to [uniq_label]
        a label that is distinct from that of any handle stored in [self] and
        distinct from that returned from any subsequent call to
        [self#unique_label].

        [store my_handle] associates [my_handle] with [uniq_label] so that it
        will be {!fold}ed over.

        It is an error to call [store] with a handle whose label is not equal to
        [uniq_label].
    *)


    method private start_to_key s : (GMap.key * R.meta_t G.Start.t) =
      let start_body = G.Start.to_body g s in
      (* Apply variants to the start.  Assumes derive is idempotent *)
      let start_node = GrammarVariant.Set.fold (GrammarVariant.derive)
        variants (G.N start_body) in
      let start_body = match start_node with
        | G.N b -> b
        | _     -> failwith "not body" in
      (start_body, variants),
      (G.Start.contextualize g (G.Start.of_body start_body))

    method private start_to_label start alt =
      let start = G.Start.contextualize g start in
      let label = match G.Start.name start with
        | Some name -> Label.of_identifier name
        | None      ->
          let pos = R.source_pos (G.body_meta (G.Start.to_body g start)) in
          (match pos.SourcePosition.scope_name with
            | Some name -> Label.of_identifier name
            | None      -> Label.of_string alt
          ) in
      self#unique_label label

    method grammar : R.meta_t G.grammar = g
    method link_to_decoder start _ =
      let k, start' = self#start_to_key start in
      if DMap.mem k tb.decs then
        DMap.find k tb.decs
      else begin
        let signature = SignatureInference.of_grammar g `Dec start' in
        let lbl, store = self#start_to_label start "decoder" in
        let handle, satisfy, break = DecoderHandle.make lbl signature in
        store (ToolUnion.Dec handle);
        tb.decs <- DMap.add k handle tb.decs;
        begin
          try
            satisfy (DecoderCompiler.compile (self :> linker) start')
          with | exn ->
            break exn;
            raise exn
        end;
        handle
      end
    method link_to_encoder start cc =
      let k, start' = self#start_to_key start in
      if EMap.mem k tb.encs then
        EMap.find k tb.encs
      else begin
        let lbl, store = self#start_to_label start "encoder" in
        let signature = SignatureInference.of_grammar g `Enc start' in
        let handle, satisfy, break = EncoderHandle.make lbl signature in
        store (ToolUnion.Enc handle);
        tb.encs <- EMap.add k handle tb.encs;
        begin
          try
            satisfy (EncoderCompiler.enc_to_il self#grammar start' cc)
          with | exn ->
            break exn;
            raise exn
        end;
        handle
      end
    method link_to_sanitizer start _ =
      let k, start' = self#start_to_key start in
      if SMap.mem k tb.sans then
        SMap.find k tb.sans
      else begin
        let lbl, store = self#start_to_label start "sanitizer" in
        let signature = SignatureInference.of_grammar g `San start' in
        let handle, satisfy, break = SanitizerHandle.make lbl signature in
        store (ToolUnion.San handle);
        tb.sans <- SMap.add k handle tb.sans;
        begin
          try
            let san = SanitizerCompiler.compile (self :> linker) start' in
            satisfy san
          with | exn ->
            break exn;
            raise exn
        end;
        handle
      end
    method link_to_contexter start _ =
      let k, start' = self#start_to_key start in
      if CMap.mem k tb.cons then
        CMap.find k tb.cons
      else begin
        let lbl, store = self#start_to_label start "contexter" in
        (* TODO: use a proper signature once contexters become
           first-class tools. *)
        let faux_signature = {
          Signature.
          (* HACK: we fake the tool kind here so that things fit. Ugly. *)
          kind    = `San;
          formals = [];
        } in
        let handle, satisfy, break = ContexterHandle.make lbl faux_signature in
        store (ToolUnion.Con handle);
        tb.cons <- CMap.add k handle tb.cons;
        begin
          try
            let con = ContexterCompiler.compile (self :> linker) start' in
            satisfy con
          with | exn ->
            break exn;
            raise exn
        end;
        handle
      end
    method apply_decoder   dec inp =
      DecoderInterp.apply   (Handle.require dec) inp
    method apply_encoder   enc inp =
      EncInterp.apply_enc   (Handle.require enc) inp
    method apply_sanitizer san inp =
      SanitizerInterp.apply (Handle.require san) inp
    method apply_contexter con inp =
      ContexterInterp.apply (Handle.require con) inp

    method lookup lbl = match Label.Map.find_opt lbl tb.labels with
      | Some x -> x
      | None   -> None

    method fold f x = Label.Map.fold
      (fun _ h_opt x -> match h_opt with
        | None   -> x
        | Some h -> f x h)
      tb.labels x

    method source_pos x = R.source_pos x
  end

  let make g : R.meta_t Linker.t =
    new linker
      g
      {
        decs     = DMap.empty;
        encs     = EMap.empty;
        sans     = SMap.empty;
        cons     = CMap.empty;
        labels   = Label.Map.empty;
        variants = GrammarVariantSetMap.singleton GrammarVariant.Set.empty g;
      }
      GrammarVariant.Set.empty

end
