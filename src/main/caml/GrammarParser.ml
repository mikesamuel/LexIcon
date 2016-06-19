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

module CF = CaseFold
module G = Grammar
module Range = Unicode.Range
module Namespace = Identifier.Namespace

let (=@)  = Unicode.Cmp.(=@)
let (<@>) = Unicode.Cmp.(<@>)
let (<@)  = Unicode.Cmp.(<@)
let (<=@) = Unicode.Cmp.(<=@)

type 'meta grammar_body = 'meta G.grammar_body =
  | Annotation of 'meta * 'meta G.annotation * 'meta grammar_body
  | CharSet of 'meta * Unicode.Range.Set.t
  | Concatenation of 'meta * 'meta grammar_body list
  | Difference of 'meta * 'meta grammar_body * 'meta grammar_body
  | Reference of 'meta * Identifier.t
  | Repetition of 'meta * 'meta grammar_body
  | Union of 'meta * G.Ordering.t * 'meta grammar_body list
  | Panic of 'meta

type 'meta annotation = 'meta G.annotation =
  | Data of POD.t
  | Denormalized of 'meta grammar_body option * Var.Pred.t
  | Embedded of 'meta grammar_body * Var.Pred.t
  | CaseFold of CF.t
  | Scope of Var.Name.t * G.Recursivity.t
  | Set of Var.Name.t * Var.Expr.t
  | If of Var.Pred.t
  | Until of 'meta grammar_body
  | Override of Identifier.t * 'meta grammar_body
  | Entrust of Identifier.t * Var.Names.t * Var.Pred.t

and recursivity = G.Recursivity.t = Flat | Recursive

type 'meta production = 'meta G.production =
    Production of 'meta * Identifier.t * 'meta grammar_body

type 'meta grammar = 'meta G.grammar =
  Grammar of 'meta * 'meta G.headers * 'meta production list

type 'meta node = 'meta G.node =
  | A of 'meta annotation
  | G of 'meta grammar
  | P of 'meta production
  | N of 'meta grammar_body


let c2uni = Unicode.c2uni
let i2uni = Unicode.i2uni
let uni2c = Unicode.uni2c
let uni2i = Unicode.uni2i
module Uni = Unicode

let sprintf = Printf.sprintf

type precedence =
  | GrammarPrec
  | ProdPrec
  | UnionPrec
  | ConcatenationPrec
  | AnnotationPrec
  | PostfixPrec
  | DifferencePrec
  | AtomPrec

let cmp_prec =
  let module Cmp = MakeSimpleCmp (struct type comparable = precedence end) in
  Cmp.compare

let esc_char_for_dq_string =
  Uni.escape ~extra_escs:[c2uni '[', "["; c2uni ']', "]"]

let collector () =
  let toks = ref [] in
  ((fun tok -> toks := tok::!toks),
   (fun () -> List.rev !toks))

let comment =
  let block_comment_end = Str.regexp "\\*/" in
  let line_break = Str.regexp "^" in
  fun out toks break_after ->
    let coll_out, coll_toks = collector () in
    let coll_out, finish = Stringer.indenter coll_out in
    List.iter coll_out toks;
    finish ();
    let str = String.concat "" (coll_toks ()) in
    try
      ignore (Str.search_forward block_comment_end str 0);
      out (Str.global_replace line_break "// " str);
      out "\n"
    with | Not_found ->
      out (sprintf "/* %s */" str);
      if break_after then out "\n"

let resugar_negative_lookahead b = match b with
  | Annotation (
    _,
    Scope (n0, _),
    Concatenation (_, [
      Union (_, _, [
        Annotation (_, Set (n1, Var.Expr.Val (Var.Value.One s1)), cond);
        Annotation (_, Set (n2, Var.Expr.Val (Var.Value.One s2)),
                    Concatenation (_, []));
      ]);
      Annotation (_, If (Var.Pred.Any (n3, syms)), Concatenation (_, []));
    ])
  ) ->

    if (
      Var.Name.equal n0 n1
      && Var.Name.equal n0 n2
      && not (Var.Symbol.equal s1 s2)
      && Var.Name.equal n0 n3
      && Var.Symbols.mem s2 syms
      && not (Var.Symbols.mem s1 syms)) then
      Some cond
    else
      None
  | _ -> None


let desugar_negative_lookahead var_name meta end_meta b = begin
  let symbol_fail = VarsWellKnown.sym_fail in
  let symbol_pass = VarsWellKnown.sym_pass in
  let passed = Var.Pred.Any (var_name, Var.Symbols.singleton symbol_pass) in

  let expr_one sym = Var.Expr.Val (Var.Value.One sym) in

  let nop _ = Concatenation (end_meta (), []) in

  Annotation (
    meta (), Scope (var_name, Flat),
    Concatenation (meta (), [
      Union (meta (), G.Ordering.Ordered, [
        Annotation (end_meta (), Set (var_name, expr_one symbol_fail), b);
        Annotation (end_meta (), Set (var_name, expr_one symbol_pass), nop ());
      ]);
      Annotation (  end_meta (), If  passed,    nop ());
    ])
  )
end


let resugar_embed_body x = match x with
  | Union (_, _, [Annotation (_, Set (n, Var.Expr.Val v), body); Panic _])
      when (Var.Value.equal VarsWellKnown.val_pass v) ->
    (* Not yet completely desugared by PreSimplify *)
    body, Some n
  | (
    Union (_, _, [
      Annotation (_, Set (n0, Var.Expr.Val v0), body);
      Annotation (_, Set (n1, Var.Expr.Val v1),
                  Union (_, _, [
                    Repetition (_, CharSet (_, any_char));
                    Concatenation (_, [])
                  ]))
    ])
  )
      when (Var.Name.equal n0 n1
            && Var.Value.equal VarsWellKnown.val_pass v0
            && Var.Value.equal VarsWellKnown.val_fail v1) ->
    (* TODO: maybe check that any_char spans all code-units potentially
       handled by body. *)
    let body_without_end_of_input_assertion = match body with
      | G.Concatenation (cm, ls) -> (match List.rev ls with
          | hd_rev::tl_rev -> (match resugar_negative_lookahead hd_rev with
              | Some (G.CharSet (_, la_char))
                  when Unicode.Range.Set.equal any_char la_char ->
                (match tl_rev with
                  | [x] -> x
                  | _   -> G.Concatenation (cm, List.rev tl_rev))
              | _ -> body)
          | [] -> body
      )
      | _ -> body
    in
    body_without_end_of_input_assertion, Some n0
  | Union (_, _, [body; Panic _]) ->
    body, None
  | _ -> x, None


let rec body_stringer str_meta prec out node =
  let body_out, body_toks = collector () in
  let child_stringer cprec c = body_stringer str_meta cprec body_out c in
  let has_empty_meta node =
    let empty_meta = ref true in
    str_meta (fun _ -> empty_meta := false) (G.body_meta node);
    !empty_meta in
  let only_desc_has_non_empty_meta anc desc =
    let rec walk empty_meta n =
      empty_meta && (
        match n with
          | G.N anc when same anc desc -> true
          | G.N anc when not (has_empty_meta anc) -> false
          | _ -> G.fold walk empty_meta n) in
    walk true (G.N anc) in
  let meta_toks =
    let meta_out, meta_toks = collector () in
    str_meta meta_out (G.body_meta node);
    meta_toks () in
  let out_prec = match node with
    | Annotation (_, annot, body) ->
      (match resugar_negative_lookahead node with
        | Some neg_la_body when only_desc_has_non_empty_meta node neg_la_body ->
          body_out "!";
          body_out Stringer.no_break;
          child_stringer PostfixPrec neg_la_body;
          PostfixPrec
        | _ ->
          annot_stringer str_meta body_out annot;
          child_stringer AnnotationPrec body;
          AnnotationPrec)
    | CharSet (_, ranges) ->
      let diff_str = ref "" in
      let str = Range.Set.to_string
        ~combine:(
          fun bound ranges ->
            let cs = "[" ^ (String.concat "" ranges) ^ "]" in
            match bound with
              | None -> cs
              | Some r ->
                let diff = "[" ^ (Range.esc_range r) ^ "]-" ^ cs in
                diff_str := diff;
                diff)
        ~range_to_string:Range.esc_range
        ~bound:(
          fun m ->
            let n = Range.Map.size m in
            if n = 0 then None
            else
              let rt = Range.Map.right m (n-1) in
              if rt <=@ (i2uni 0x100) then
                Some (Range.make Uni.zero (i2uni 0x100))
              else if rt <=@ (i2uni 0x10000) then
                Some (Range.make Uni.zero (i2uni 0x10000))
              else if rt <=@ (i2uni 0x110000) then
                Some (Range.make Uni.zero (i2uni 0x110000))
              else None)
          ranges in
      body_out str;
      if str_eq str !diff_str then DifferencePrec else AtomPrec
    | Concatenation (_, children) ->
      (* Treat runs of single character CharSets as quoted strings *)
      let is_string_part child = match child with
        | CharSet (_, ranges) when Range.Set.is_singleton ranges ->
          has_empty_meta child
        | _ -> false in
      let emit_str str_chars_rev = body_out (
        "\""
        ^ (String.concat ""
            (List.rev_map
               (fun range -> esc_char_for_dq_string (Range.Map.left range 0))
               str_chars_rev))
        ^ "\"") in
      let rec cat_str str_chars broke children =
        match children with
        | [] -> (match str_chars with
            | [] -> ConcatenationPrec
            | _ ->
              emit_str str_chars;
              if broke then ConcatenationPrec else AtomPrec)
        | ((CharSet (_, ranges)) as cs)::tail when is_string_part cs ->
          cat_str (ranges::str_chars) broke tail
        | child::tail ->
          (match str_chars with
            | [] -> ()
            | _ -> emit_str str_chars);
          child_stringer ConcatenationPrec child;
          cat_str [] true tail in
      cat_str [] false children
    | Difference (_, minuend, subtrahend) ->
      child_stringer DifferencePrec minuend;
      body_out Stringer.no_break;
      body_out "-";
      body_out Stringer.no_break;
      child_stringer DifferencePrec subtrahend;
      DifferencePrec
    | Reference (_, name) -> Identifier.stringer body_out name; AtomPrec
    | Panic _ -> body_out "_PANIC_"; AtomPrec
    | Repetition (_, body) ->
      child_stringer PostfixPrec body;
      body_out Stringer.no_break;
      body_out "+";
      PostfixPrec
    (* We ignore ordering when printing unions since ordering is a property
       proven about a grammar and is not directly manipulable by authors. *)
    | Union (_, _, []) ->
      (* () is ambiguous with Concatenation so we represent the zero options
         as a charset of zero ranges. *)
      body_out "["; body_out "]";
      AtomPrec
    | Union (_, _, [Repetition (_, body) as r; Concatenation (_, []) as c])
        when has_empty_meta r && has_empty_meta c ->
      child_stringer PostfixPrec body;
      body_out Stringer.no_break;
      body_out "*";
      PostfixPrec
    | Union (_, _, [body; Concatenation (_, []) as c]) when has_empty_meta c ->
      child_stringer PostfixPrec body;
      body_out Stringer.no_break;
      body_out "?";
      PostfixPrec
    | Union (_, _, children) ->
      let rec maybe_qmark child_list = match child_list with
        | [] -> failwith "unreachable"
        | [Concatenation (_, [])] -> Some []
        | [_] -> None
        | hd::tl -> (match maybe_qmark tl with
          | Some prev -> Some (hd::prev)
          | None -> None) in
      let rec bars_between children = match children with
        | [] -> failwith "unreachable"
        | [child] ->
          child_stringer UnionPrec child
        | child::tl ->
          child_stringer UnionPrec child;
          body_out "|";
          bars_between tl in
      match maybe_qmark children with
        | Some (_::_::_ as all_but_last) ->
          (* (a | b | ()) can be abbreviated to (a | b)? *)
          body_out "(";
          bars_between all_but_last;
          body_out ")";
          body_out Stringer.no_break;
          body_out "?";
          PostfixPrec
        | _ ->
          bars_between children;
          UnionPrec in

  let body_toks = body_toks () in
  match meta_toks, body_toks with
    | [], [] -> out "("; out ")"
    | [], _ ->
      if cmp_prec out_prec prec <= 0 then begin
        out "(";
        List.iter out body_toks;
        out ")";
      end else
        List.iter out body_toks
    | _, _ ->
      out "(";
      List.iter out body_toks;
      comment out meta_toks false;
      out ")"

and annot_ctor name out f =
  out name;
  out Stringer.no_break;
  out "{";
  out Stringer.no_break;
  f out;
  out Stringer.no_break;
  out "}"

and annot_stringer str_meta out annot = match annot with
  | Data t -> POD.stringer out t
  | Denormalized (None, p) when Var.Pred.equal p Var.Pred._true ->
    out "@Denormalized"
  | Denormalized (s, p) ->
    let is_elide = match s with
      | Some (Concatenation (_, [])) -> true
      | _                            -> false in
    if is_elide && Var.Pred.equal p Var.Pred._true then
      out "@Elide"
    else
      annot_ctor (if is_elide then "@Elide" else "@Denormalized") out
        (fun o ->
          if not is_elide then
            (match s with
              | Some b -> body_stringer str_meta ProdPrec o b
              | _ -> ());
          if not (Var.Pred.equal p Var.Pred._true) then begin
            o ":";
            Var.Pred.stringer o p
          end)
  | Embedded (x, p) ->
    let body, status_var_opt = resugar_embed_body x in
    annot_ctor "@Embedded" out
      (fun o ->
        body_stringer str_meta ProdPrec o body;
        Opt.iter (fun x -> o ","; Var.Name.stringer o x) status_var_opt;
        if not (Var.Pred.equal p Var.Pred._true) then begin
          o ":";
          Var.Pred.stringer o p
        end)
  | CaseFold cf -> out (match cf with
    | CF.CaseFoldNone -> "@CaseFoldNone"
    | CF.CaseFold7Bit -> "@CaseFold7Bit")
  | Scope (name, Flat) ->
    annot_ctor "@Scope" out (fun o -> Var.Name.stringer o name)
  | Scope (name, r) ->
    annot_ctor "@Scope" out
      (fun o -> Stringer.tup2 Var.Name.stringer G.Recursivity.stringer
        o (name, r))
  | If p ->
    annot_ctor "@If" out (fun o -> Var.Pred.stringer o p)
  | Set (n, e) ->
    annot_ctor "@Set" out
      (fun o -> Var.Name.stringer o n; o ","; Var.Expr.stringer o e)
  | Until x ->
    annot_ctor "@Until" out (fun o -> body_stringer str_meta ProdPrec o x)
  | Override (name, node) ->
    annot_ctor "@Override" out
      (fun o ->
        Identifier.stringer o name;
        o ",";
        body_stringer str_meta ProdPrec o node)
  | Entrust (name, read, p) ->
    annot_ctor "@Entrust" out
      (fun o ->
        Identifier.stringer o name;
        Var.Names.iter
          (fun nm ->
            o ",";
            Var.Name.stringer o nm)
          read;
        if not (Var.Pred.equal p Var.Pred._true) then begin
          o ":";
          Var.Pred.stringer o p
        end)

let prod_stringer str_meta out (Production (m, name, body)) =
  let meta_out, meta_toks = collector () in
  str_meta meta_out m;
  (match meta_toks () with
    | [] -> ()
    | toks -> comment out toks true);
  Identifier.stringer out name;
  out ":=";
  out " ";
  body_stringer str_meta ProdPrec out body

let make_headers_stringer ?(str_meta=fun _ _ -> ()) out
    { G.namespace; grammar_variables } =
  let _ = str_meta in
  out "{";
  if not (Namespace.equal namespace Namespace.default) then begin
    out "namespace"; out "=";
    Namespace.stringer out namespace;
    out ";";
  end;
  let module SymbolDeclsMap = MapUtil.Make (struct
    type t = Var.Symbol.t option list
    let compare = ListUtil.compare (Opt.compare Var.Symbol.compare)
    let stringer = Stringer.ignore
  end) in
  let aliases_ref = ref SymbolDeclsMap.empty in
  Var.Map.iter
    (fun name (_, domain) ->
      Var.Name.stringer out name;
      out "<:";
      let syms, plural =
        (match domain with
          | Var.Domain.One  ls -> ls, false
          | Var.Domain.Many ls -> ls, true) in
      let syms = List.map (Opt.map snd) syms in
      let aliases = !aliases_ref in
      if SymbolDeclsMap.mem syms aliases then
        let name = SymbolDeclsMap.find syms aliases in
        Var.Name.stringer out name
      else begin
        aliases_ref := SymbolDeclsMap.add syms name aliases;
        out "(";
        ignore (
          List.fold_left
            (fun need_comma sym_opt ->
              if need_comma then out ",";
              (match sym_opt with
                | None     -> out "_"
                | Some sym -> Var.Symbol.stringer out sym);
              true)
            false syms
        );
        out ")"
      end;
      if plural then out "*";
      out ";"
    )
    (Var.Decls.as_map grammar_variables);
  out "}"

let headers_stringer o x = make_headers_stringer o x

let make_grammar_stringer ?(str_meta=fun _ _ -> ()) ?(include_headers=false)
    out (Grammar (m, hdrs, prods)) =
  let meta_out, meta_toks = collector () in
  str_meta meta_out m;
  (match meta_toks () with
    | []    -> ()
    | toks  -> comment out toks true; out "\n");
  let rec stringify prods = match prods with
    | []    -> ()
    | [p]   -> prod_stringer str_meta out p
    | p::tl -> prod_stringer str_meta out p; out ";"; out "\n"; stringify tl in
  if include_headers then begin
    make_headers_stringer ~str_meta out hdrs;
    if not (is_empty prods) then
      out "\n";
  end;
  stringify prods

let grammar_stringer o x = make_grammar_stringer o x

(* Above we use non-optional parameters to avoid failing to thread a parameter
   through, but here we mask functions to supply defaults. *)
let make_body_stringer ?(str_meta=fun _ _ -> ()) ?(prec=ProdPrec) out body =
  body_stringer str_meta prec out body

let body_stringer o x = make_body_stringer o x

let make_annot_stringer ?(str_meta=fun _ _ -> ()) out annot =
  annot_stringer str_meta out annot

let annot_stringer o x = make_annot_stringer o x

let make_prod_stringer ?(str_meta=fun _ _ -> ()) out prod =
  prod_stringer str_meta out prod

let prod_stringer o x = make_prod_stringer o x

let make_stringer ?(str_meta=fun _ _ -> ()) out node = match node with
  | N x -> make_body_stringer    ~str_meta:str_meta out x
  | A x -> make_annot_stringer   ~str_meta:str_meta out x
  | P x -> make_prod_stringer    ~str_meta:str_meta out x
  | G x -> make_grammar_stringer ~str_meta:str_meta out x

let stringer o x = make_stringer o x

let make_start_stringer ?(str_meta=fun _ _ -> ()) out start = match start with
  | Grammar.Start.Named name ->
    Stringer.ctor "Named" Identifier.stringer                           out name
  | Grammar.Start.Body  body ->
    Stringer.ctor "Body"  (fun o x -> make_body_stringer ~str_meta o x) out body

let start_stringer o x = make_start_stringer o x

type token_status = Complete | Truncated

let is_digit ch = (c2uni '0') <=@ ch && ch <=@ (c2uni '9')

let is_identifier_start ch =
  (c2uni 'A') <=@ ch && ch <=@ (c2uni 'Z')
  || (c2uni 'a') <=@ ch && ch <=@ (c2uni 'z')
  || ch =@ (c2uni '_')

let is_identifier_part ch = is_identifier_start ch || is_digit ch


module AnnotationParam = struct
  type kind =
    | BodyKind
    | ValueExprKind
    | PredKind

  type part =
    | Body of SourcePosition.t grammar_body
    | Int  of SourcePosition.t * int
    | Pred of SourcePosition.t * Var.Pred.t
    | Expr of SourcePosition.t * Var.Expr.t
end

module AnnotationMakers : sig

  val make_annotation :
       SourcePosition.t G.headers
    -> SourcePosition.t -> string -> AnnotationParam.part list
    -> SourcePosition.t grammar_body -> SourcePosition.t grammar_body
  (** [make_annotation pos name parameters body] *)

  val param_kind : string -> int -> AnnotationParam.kind

end = struct

  type annot_part = AnnotationParam.part =
    | Body of SourcePosition.t grammar_body
    | Int  of SourcePosition.t * int
    | Pred of SourcePosition.t * Var.Pred.t
    | Expr of SourcePosition.t * Var.Expr.t

  module StringMap = MapUtil.StringMap

  (* Parameter type conversions *)

  let annot_part_meta p = match p with
    | Body x -> G.body_meta x
    | Int  (m, _)
    | Pred (m, _)
    | Expr (m, _) -> m

  let annot_part_string p = match p with
    | Body x      -> Stringer.s body_stringer x
    | Int  (_, i) -> string_of_int i
    | Pred (_, p) -> Stringer.s Var.Pred.stringer p
    | Expr (_, e) -> Stringer.s Var.Expr.stringer e

  let bad_syntax fmt param =
      raise (Failures.Bad_syntax
               (annot_part_meta param,
                sprintf fmt (annot_part_string param)))

  let id param = match param with
    | Body (Reference (_, name)) -> name
    | _ ->
      bad_syntax "Expected identifier not (%s)" param

  let nm param =
    let name = id param in
    if Var.Name.is_name name then
      Var.Name.make name
    else
      bad_syntax "Expected var not (%s)" param

  let nms params = Var.Names.of_list (List.map nm params)

  (* An optional var name. *)
  let no = Opt.map nm

  let rcs param_opt = match param_opt with
    | None -> Flat
    | Some (Body (Reference (_, r)))
        when str_eq (Identifier.local_name r) "rec"  -> Recursive
    | Some (Body (Reference (_, r)))
        when str_eq (Identifier.local_name r) "flat" -> Flat
    | Some param -> bad_syntax "Expected (rec) or (flat) not (%s)" param

  let num param = match param with
    | Int (_, i) -> i
    | _          -> bad_syntax "Expected integer not (%s)" param

  let any param = match param with
    | Body b -> b
    | _      -> bad_syntax "Expected grammar not (%s)" param

  let prd param = match param with
    | Pred (_, p) -> p
    | _           -> bad_syntax "Expected predicate not (%s)" param

  let uni param = match param with
    | Body (CharSet (_, cs)) when Unicode.Range.Set.is_singleton cs ->
      Opt.require (Unicode.Range.Map.min cs)
    | _ -> bad_syntax "Expected single character not (%s)" param

  let zero f pos name params = match params with
    | [] -> f ()
    | _  ->
      raise (Failures.Bad_syntax
               (pos, "Too many parameters for annotation @" ^ name))

  let opt p0 f pos name params = match params with
    | []  -> f None
    | [x] -> f (Some (p0 x))
    | _   ->
      raise (Failures.Bad_syntax
               (pos, "Too many parameters for annotation @" ^ name))

  let one p0 f pos name params = match params with
    | [x] -> f (p0 x)
    | _   ->
      raise (Failures.Bad_syntax
               (pos, "Expected one parameter for annotation @" ^ name))
  let two p0 p1 f pos name params = match params with
    | [x; y] -> f (p0 x) (p1 y)
    | _ ->
      raise (Failures.Bad_syntax
               (pos, "Expected two parameters for annotation @" ^ name))

  (* Two or one *)
  let twon p0 p1 f pos name params = match params with
    | [x]    -> f (p0 x) (p1 None)
    | [x; y] -> f (p0 x) (p1 (Some y))
    | _      ->
      raise (Failures.Bad_syntax
               (pos, "Expected one or two parameters for annotation @" ^ name))

  let more p0 pi f pos name params = match params with
    | hd::tl ->
      f (p0 hd) (pi tl)
    | []     ->
      raise (Failures.Bad_syntax
               (pos, "Expected one or more parameters for annotation @" ^ name))

  let prd0, prd1 =
    let rec strip_from_right params = match params with
      | []            -> [], Var.Pred._true
      | [Pred (_, p)] -> [], p
      | hd::tl        ->
        let tl', p = strip_from_right tl in
        hd::tl', p in
    let prd0 f pos name params =
      let params', p = strip_from_right params in
      zero (fun _ -> f pos p) pos name params' in
    let prd1 left f pos name params =
      let params', p = strip_from_right params in
      left (fun x -> f x p) pos name params' in
    prd0, prd1

  let xpr param = match param with
    | Expr (_, expr) -> expr
    | _              -> bad_syntax "Expected value expression, not (%s)" param

  let empty pos = Some (Concatenation (pos, []))

  let emb body status_var_opt =
    let meta = G.body_meta body in
    let pass, fail = match status_var_opt with
      | None -> body, G.Panic meta
      | Some status_var ->
        (* @Set{status_var, pass} body *)
        G.Annotation (
          meta,
          Set (status_var, Var.Expr.Val VarsWellKnown.val_pass),
          body),
        (* | Panic. *)
        (* PreSimplify will finish the desugaring this to
             @Set{status_var, fail} char*
           once we can reliably figure out the code unit kind to do ( char* )
           in the right namespace. *)
        G.Panic meta
    in
    Union (meta, G.Ordering.Ordered, [pass; fail])

  let maker_map = StringMap.of_list [
    "String",             zero         (fun _     -> Data POD.String);
    "Char",               zero         (fun _     -> Data POD.Char);
    "CharValue",          opt  uni     (fun x     -> Data (POD.CharValue x));
    "ScalarValue",        opt  num     (fun x     -> Data (POD.ScalarValue x));
    "KeyValueMap",        zero         (fun _     -> Data POD.KeyValueMap);
    "Key",                zero         (fun _     -> Data POD.Key);
    "Value",              zero         (fun _     -> Data POD.Value);
    "List",               zero         (fun _     -> Data POD.List);
    "Element",            zero         (fun _     -> Data POD.Element);
    "ValueFalse",         zero         (fun _     -> Data POD.ValueFalse);
    "ValueTrue",          zero         (fun _     -> Data POD.ValueTrue);
    "ValueNull",          zero         (fun _     -> Data POD.ValueNull);
    "Number",             zero         (fun _     -> Data POD.Number);
    "Denormalized", prd1 (opt any)     (fun x p   -> Denormalized (x, p));
    "Embedded",     prd1 (twon any no) (fun x p v -> Embedded (emb x v, p));
    "Implied",      prd1 (one any)     (fun x p   -> Denormalized (Some x, p));
    "CaseFold7Bit",       zero         (fun _     -> CaseFold(CF.CaseFold7Bit));
    "CaseFoldNone",       zero         (fun _     -> CaseFold(CF.CaseFoldNone));
    "Scope",              twon nm  rcs (fun n r   -> Scope (n, r));
    "Set",                two  nm  xpr (fun n e   -> Set (n, e));
    "If",                 one  prd     (fun p     -> If p);
    "Until",              one  any     (fun x     -> Until x);
    "Override",           two  id  any (fun n b   -> Override (n, b));
    "Elide",        prd0               (fun s p   -> Denormalized (empty s, p));
    "Entrust",      prd1 (more id  nms)(fun x p n -> Entrust (x, n, p));
  ]

  let fixup_map = StringMap.of_list [
    ("Implied",
     (* A sanity check. *)
     fun _ pos annot body -> match body with
       | Concatenation (_, []) -> Annotation (pos, annot, body)
       | _ ->
         raise (Failures.Bad_syntax (
           pos,
           sprintf
             "Expected empty body not (%s) for annotation @Implied"
             (Stringer.s body_stringer body))));
    ("Set",
     (* Make sure that the plurality of values matches the plurality of the
        the name to which they are assigned.
        We massage singular values to plural where necessary and panic on
        unambiguously wrong values. *)
     fun { G.grammar_variables=gvars; _ } pos annot body -> match annot with
       | Set (lhs, rhs) ->
         let domain_opt = Var.Decls.domain gvars lhs in
         let is_plural = match domain_opt with
           | Some (Var.Domain.Many _) -> true
           | Some (Var.Domain.One  _) -> false
           (* Plural variables have to be explicitly declared thus. *)
           | None                     -> false in
         let rec fixup_expr e = match e with
           | Var.Expr.Val (Var.Value.Many _) when not is_plural ->
             raise (Failures.Bad_syntax (
               pos,
               sprintf "Expected singular value for %s, not %s.  %s"
                 (Stringer.s Var.Name.stringer lhs)
                 (Stringer.s Var.Expr.stringer rhs)
                 (match domain_opt with
                   | None -> "Maybe you need to declare the variable"
                   | _    ->
                     "Maybe you need a `*` after its domain in the header")
             ))
           | Var.Expr.Val (Var.Value.One s) when is_plural ->
             Var.Expr.Val (Var.Value.Many (Var.Symbols.singleton s))
           | Var.Expr.Val _ -> e
           | Var.Expr.Ref referent ->
             (match Var.Decls.domain gvars referent with
               | Some (Var.Domain.Many _) when not is_plural ->
                 raise (Failures.Bad_syntax (
                   pos,
                   sprintf
                     ("Expression for singular variable %s refers"
                      ^^ " to plural variable %s%s")
                     (Stringer.s Var.Name.stringer lhs)
                     (Stringer.s Var.Name.stringer referent)
                     (match Var.Decls.name_meta gvars referent with
                       | None -> ""
                       | Some decl_pos ->
                         sprintf " defined at %s"
                           (Stringer.s SourcePosition.stringer decl_pos))
                 ))
               | Some (Var.Domain.Many _)
               | Some (Var.Domain.One  _)
               | None                     -> e)
           | Var.Expr.Nin ls -> Var.Expr.Nin (List.map fixup_expr ls) in
         let rhs' = fixup_expr rhs in
         Annotation (pos, Set (lhs, rhs'), body)
       | _ -> failwith "unreachable"
    );
  ]

  let make_annotation headers pos name params body =
    if str_eq name "Lit" then
      (* Allow embedding a literal string value via desugaring. *)
      (* @Lit{"foo"} bar ->
         bar @CharValue{"f"}() @CharValue{"o"}() @CharValue{"o"}() *)
      let rec implied_value_chars n = match n with
        | CharSet (c_pos, r) ->
          if Unicode.Range.Set.is_singleton r then
            Some (
              Annotation (
                c_pos, Data POD.Char,
                Annotation (
                  c_pos, Data (POD.CharValue (Unicode.Range.Map.min r)),
                  Concatenation (SourcePosition.end_of c_pos, []))))
          else
            raise (Failures.Bad_syntax (c_pos, "Expected single char"))
        | Concatenation (m, els)  ->
          let els' = List.fold_right
            (fun el ls_opt -> match implied_value_chars el, ls_opt with
              | Some hd, Some tl  -> Some (hd::tl)
              | _                 -> None)
            els (Some []) in
          Opt.map (fun els' -> Concatenation (m, els')) els'
        | Union (_, _, [el])      -> implied_value_chars el
        | _                       -> None in
      let chars = match params with
        | [Body param] -> implied_value_chars param
        | _            -> None in
      match chars with
        | Some chars -> Concatenation (pos, [body; chars])
        | None       ->
          raise (Failures.Bad_syntax (pos, "Expected one string parameter"))
    else if str_eq name "EachChar" then
      (* The macro
            @EachChar{ name, substitution } body
         expands to body, but with every CharSet replaced with substitution
         where each and every reference to name in substitution is replaced with
         the CharSet so
             @EachChar{ x, (char - x) } [f] [o] [o]
         expands to
             [^f] [^o] [^o]
       *)
      match params with
        | [Body (Reference (_, var)); Body replacement] ->
          let rec subst n = match n with
            | N (CharSet _ as cs) ->
              let rec expand_var n = match n with
                | N (Reference (_, name)) when Identifier.equal name var -> N cs
                | _ -> G.map_children expand_var n in
              expand_var (N replacement)
            | _ -> G.map_children subst n in
          (match subst (N body) with | N x -> x | _ -> failwith "mismatch")
        | _ ->
          raise (Failures.Bad_syntax (
            pos,
            "Expected as parameters one identifier"
            ^ " and one replacement that uses the identifier"))
    else if StringMap.mem name maker_map then begin
      let maker = StringMap.find name maker_map in
      let annot = maker pos name params in
      (match StringMap.find_opt name fixup_map with
        | None       -> Annotation (pos, annot, body)
        | Some fixer -> fixer headers pos annot body)
    end else
      let suggestion, _ = StringMap.fold
        (fun k _ (best, best_dist) ->
          let k_dist = EditDistance.distance name k in
          if k_dist < best_dist then
            k, k_dist
          else
            best, best_dist)
        maker_map
        ("", max_int) in
      raise (Failures.Bad_syntax (
        pos,
        "Unrecognized annotation @" ^ name ^ ", did you mean @"
        ^ suggestion))

  let param_kind name index = match name, index with
    | "If",  0 -> AnnotationParam.PredKind
    | "Set", 1 -> AnnotationParam.ValueExprKind
    | _        -> AnnotationParam.BodyKind

end

module ImportPrefixHeuristic : sig val choose : string -> string end = struct
  let path_separator_re = Str.regexp "[/\\\\]"
  let non_ident_char_re = Str.regexp "[^a-zA-Z0-9_]+"
  let word_grammar_at_end_re =
    Str.regexp "_*[Gg][Rr][Aa][Mm][Mm][Aa][Rr][Ss]?_*$"
  let file_suffix = Str.regexp "\\.[^.]+$"

  let choose path =
    (* Use a prefix formed from path by
       1. Stripping off any file suffix since they are low-entropy.
     2. Stripping off anything before a "." or ".." path element so that we
        are dealing with a path suffix relative to a root directory.
     3. Remove any characters that are not ascii letters or digits or '_' or
        path separators ('/' or '\\') to make it a valid path of
        identifier chars.
     4. Strip path separators from the front and end.
     5. Strip the word grammar from the end (case-insensitively) since it is
        a low entropy suffix.
     6. Strip path elements until the string has length less than 16 or it
        has one path element since long prefixes make debugging content hard to
        read.
     7. Replace any path separators ('/' or '\\') with "_" so that the prefix
        contains only valid identifier character.
     8. If the prefix is empty, replace it with "Import"
     9. If the prefix starts with a digit, prefix it with "I" for import to make
        it a valid identifier. *)
    let path_elements = Str.split path_separator_re path in  (* Step 3. *)
    let path_elements =
      List.filter (negate str_eq "") path_elements in (* Step 4 *)
    let rec trim path_elements = match path_elements with (* Steps 1 and 2 *)
      | [] -> []
      | [x] ->
        let bn = Str.replace_first file_suffix "" x in
        if str_eq bn "" then [] else [bn]
      | "."::rest | ".."::rest -> trim rest
      | hd::tl -> hd::(trim tl) in
    let path_elements = trim path_elements in
    let rec to_ident_chars path_elements = match path_elements with
      | [] -> [], 0
      | hd::tl ->
        let hd' = Str.global_replace non_ident_char_re "_" hd in  (* Step 6 *)
        let hd' = Str.global_replace word_grammar_at_end_re "" hd' in (* 5 *)
        let tl', tail_len = to_ident_chars tl in
        if str_eq hd' "" then
          tl', tail_len
        else
          let hd_len = String.length hd' in
          let full_len = (if tail_len <> 0 then tail_len + 1 else 0) + hd_len in
          if tail_len <> 0 && full_len > 16 then
            tl', tail_len
          else
            hd'::tl', full_len in
    let path_elements, _ = to_ident_chars path_elements in  (* Step 7 *)
    let prefix = String.concat "_" path_elements in
    if str_eq prefix "" then  (* Step 8 *)
      "Import"
    else
      let c0 = prefix.[0] in
      if '0' <=% c0 && c0 <=% '9' then  (* Step 9 *)
        "I" ^ prefix
      else
        prefix
end

type parser_callback =
     Path.t -> ByteInput.t -> SourcePosition.t -> SourcePosition.t grammar

type grammar_loader =
  parser_callback -> Path.t -> SourcePosition.t grammar

(* Collect information about imported grammars so that we can handle
   multiple&cyclic imports without namespace conflicts. *)
type parse_context = {
  import_depth   : int;
  ns_hint        : Namespace.t;
  loaded         : (Path.t option, Namespace.t) Hashtbl.t;
  grammar_loader : grammar_loader;
  id_counter     : (unit -> int);
  load_path      : Path.t option;
  imports        : (SourcePosition.t grammar) list ref;
}

(* A namespace used for synthetic variables created during deugaring. *)
let synthetic_ns = Namespace.synthetic

let callable_once f =
  let called = ref false in
  fun x ->
    if !called then
      failwith "Cannot be called multiple times"
    else begin
      called := true;
      f x
    end

(* A recursive descent parser for the Grammar grammar. *)
let rec parse ({ ns_hint; _ } as ctx) as_grammar inp pos =
  let source = SourcePosition.source pos in
  let scope_name = ref None in
  let line = ref (SourcePosition.start_line pos) in
  let column = ref (SourcePosition.start_col pos) in
  let lookahead = ref Uni.eof in
  let buffer_len = 4096 in
  let buffer = Bytes.make buffer_len '\x00' in
  let buffer_written = ref 0 in
  let buffer_read = ref 0 in
  let eof_reached = ref false in

  (* Position between the given line and column and cursor. *)
  let pos start_line start_col =
    SourcePosition.make source !scope_name start_line start_col !line !column in

  (* Position from the start of the given node's position and cursor. *)
  let body_pos node =
    let sp = (G.body_meta node) in
    pos (SourcePosition.start_line sp) (SourcePosition.start_col sp) in

  (* Zero length position at cursor. *)
  let cursor_pos () = pos !line !column in

  (*
     Returns the character at the cursor (or -1 if none) without changing the
     cursor.
   *)
  let rec peek () =
    let la = !lookahead in
    if (uni2i la) >= 0 then
      la
    else
      let n_avail = !buffer_written - !buffer_read in
      if n_avail < 6 && not !eof_reached then begin
        Bytes.blit buffer !buffer_read buffer 0 n_avail;
        buffer_read := 0;
        buffer_written := n_avail;
        let n_read = inp buffer n_avail (buffer_len - n_avail) in
        if n_read = 0 then
          eof_reached := true
        else
          buffer_written := n_avail + n_read;
        peek ()
      end else if n_avail <> 0 then begin
        let code_point, n_bytes = Utf8.decode
          (Bytes.to_string buffer) !buffer_read in
        let read_cursor = !buffer_read in
        if n_bytes > n_avail then
          raise (Utf8.Bad_octet (Bytes.get buffer read_cursor))
        else begin
          lookahead := code_point;
          buffer_read := read_cursor + n_bytes;
          if 0xd800 <= uni2i code_point && uni2i code_point <= 0xdbff then
            (* Look for surrogate following. *)
            (* Since we check that there are 6 bytes available and each *)
            (* surrogate takes 3 bytes in UTF-8, we should have one if *)
            (* there is one. *)
            let next, n_bytes = Utf8.decode
              (Bytes.to_string buffer) !buffer_read in
            if 0xdc00 <= uni2i next && uni2i next <= 0xdffff then begin
              buffer_read := !buffer_read + n_bytes;
              let supplemental = i2uni(
                0x10000
                + ((uni2i code_point land 0x3ff) lsl 10)
                + (uni2i next land 0x3ff)) in
              lookahead := supplemental;
              supplemental
            end else
              raise (Failures.Bad_syntax (cursor_pos (), "Orphaned surrogate"))
          else
            code_point
        end
      end else
        Uni.eof in

  (* True iff the cursor is at the end of input. *)
  let is_empty () = peek() =@ Uni.eof in

  (* Moves the cursor past the current character.
     Treats a CRLF sequence as one character. *)
  let consume () =
    let c = !lookahead in
    lookahead := Uni.eof;
    match uni2i c with
      | -1 -> ()
      | 0xa -> (column := 0; line := !line + 1)
      | 0xd -> (column := 0; line := !line + 1;
                if peek () =@ i2uni 0xa; then lookahead := Uni.eof)
      | _ -> (column := !column + 1) in

  let rec consume_while f x =
    let ch = peek () in
    if ch =@ Uni.eof then
      x
    else match f ch x with
      | Some x -> (consume (); consume_while f x)
      | _ -> x in

  let rec consume_including f x =
    let ch = peek () in
    if ch =@ Uni.eof then
      Truncated, x
    else (
      consume ();
      match f ch x with
        | Some x -> consume_including f x
        | _ -> Complete, x) in

  let fail_if_empty msg =
    if is_empty () then
      raise (Failures.Bad_syntax
             (cursor_pos (), msg ^ " but got end of input")) in

  (* True iff the character at the cursor is between significant tokens. *)
  let is_ignorable_token () =
    match uni2i (peek ()) with
      | 0x9  -> true  (* TAB *)
      | 0xa  -> true  (* LF *)
      | 0xd  -> true  (* NL *)
      | 0x20 -> true  (* space *)
      | 0x2f -> true  (* / Comment start *)
      | _    -> false in

  let skip_until_line_break () =
    ignore (consume_while
              (fun ch _ ->
                if ch <@> (c2uni '\n') && ch <@> (c2uni '\r') then
                  Some ()
                else None)
              ()) in

  let skip_until_star_slash () =
    let start_line = !line in
    let start_col = !column - 2 in
    match (consume_including
             (fun ch saw_star ->
               if ch =@ (c2uni '*') then
                 Some true
               else if saw_star && ch =@ (c2uni '/') then
                 None
               else
                 Some false)
             false) with
        | Truncated, _ ->
          raise (Failures.Bad_syntax (
            pos start_line start_col, "Expected `*/` but got end of input"))
        | _ -> () in

  (* Moves the cursor to the beginning of the next significant token. *)
  let rec skip_ignorable_tokens () =
    if is_ignorable_token () then
      let c = peek () in
      if c =@ c2uni '/' then (
        let start_line = !line in
        let start_col = !column in
        consume ();
        let c2 = peek () in
        consume ();
        if c2 =@ (c2uni '/') then (* // *)
          skip_until_line_break ()
        else if c2 =@ (c2uni '*') then (* /* *)
          skip_until_star_slash ()
        else
          raise (Failures.Bad_syntax (
            pos start_line start_col, "Expected comment start")))
      else
        consume ();
      skip_ignorable_tokens () in

  let optional node = Union (
    (body_pos node), G.Ordering.Ordered,
    [node; Concatenation (cursor_pos(), [])]
  ) in

  let expect_token tok =
    skip_ignorable_tokens ();
    let start_line = !line in
    let start_column = !column in
    let tok_len = String.length tok in
    let rec match_tok i =
      if i <> tok_len then
        let want = c2uni tok.[i] in
        let got = peek () in
        if want =@ got then (
          consume ();
          match_tok (i+1))
        else
          if got =@ Uni.eof then
            raise (Failures.Bad_syntax (
              (pos start_line start_column),
              "Expected `" ^ tok ^ "` but got end of input"))
          else
            raise (Failures.Bad_syntax (
              (pos start_line start_column),
              (sprintf
                 "Expected `%s` but got `%s%s`"
                 tok (String.sub tok 0 i) (Utf8.encode got)))) in
    match_tok 0 in

  let check_token ch =
    skip_ignorable_tokens ();
    if peek () =@ c2uni ch then (consume (); true) else false in

  (* [\-]? ( 0 | [1-9] [0-9]* ) not followed by an identifier part. *)
  let parse_integer () =
    let start_line = !line in
    let start_col = !column in
    let ch = peek() in
    (* '+' signs are silly.  Also, octal.  Care bears. *)
    let sign = (if ch =@ c2uni '-' then (consume (); ~-1) else 1) in
    fail_if_empty "Expected digit";
    let ch = peek() in
    let nat =
      if ch =@ c2uni '0' then (
        consume ();
        (* 0 can only be represented by "0" to avoid octal confusion *)
        0)
      else if is_digit ch then
        consume_while
          (fun ch preceding ->
            if is_digit ch then
              let n = (preceding * 10) + (uni2i ch) - (int_of_char '0') in
              if n < preceding then
                raise (Failures.Bad_syntax
                       (pos start_line start_col, "Underflow"));
              Some n;
            else None)
          0
      else
        raise (Failures.Bad_syntax (
          pos start_line start_col,
          sprintf "Expected digit, but got `%s`" (Utf8.encode ch))) in
    (* Make sure this doesn't bleed into an identifier token. *)
    if is_identifier_part (peek ()) then
      raise (Failures.Bad_syntax (
        pos start_line start_col, "Identifier starts with digits"))
    else
      (* nat is natural so no need to check for underflow *)
      nat * sign in

  (* Parses and returns a keyword or identifier as a string without
     first consuming any space. *)
  let parse_word kind =
    fail_if_empty ("Expected " ^ kind);
    let ch = peek () in
    if is_identifier_start (peek ()) then
      let chars_rev = consume_while
        (fun ch chars_rev ->
          if is_identifier_part ch then Some (ch::chars_rev) else None)
        [] in
      UnicodeSeq.to_utf8 (UnicodeSeq.of_list (List.rev chars_rev))
    else
      raise (Failures.Bad_syntax (
        cursor_pos (),
        Printf.sprintf "Expected start of %s but got `%s`"
          kind (Utf8.encode ch))) in

  (* Parse the header block at the start of the file so we can use decide
     on a namespace earlier which is implicitly added to all Identifiers
     in this grammar. *)
  let headers =
    let parse_headers () =
      skip_ignorable_tokens ();
      if as_grammar && peek () =@ c2uni '{' then begin
        (* Look for block of the form
             { <relation>; <relation>; <relation>; ... }
           where relations are things like
             <keyword> = <value>
           or
             <identifier> <operator> <value>
        *)
        consume ();
        let rec parse_header_body headers_seen (ns_opt, value_map) =
          skip_ignorable_tokens ();
          let start_line, start_col = !line, !column in
          let word = parse_word "keyword or variable name" in
          skip_ignorable_tokens ();
          (* Recognize a relation operator. *)
          let punc = if peek () =@ c2uni '<' then "<:" else "=" in
          expect_token punc;
          skip_ignorable_tokens ();
          (* Fail on duplicates without requiring every case below
             to do so. *)
          let headers_seen' =
            let hdr_eq (a, b) (c, d) = str_eq a c && str_eq b d in
            if List.exists (hdr_eq (word, punc)) headers_seen then
              raise (Failures.Bad_syntax (pos start_line start_col,
                "Duplicate header: " ^ word ^ " " ^ punc))
            else
              (word, punc)::headers_seen in
          (* Case-by-case handlers for relations. *)
          let headers' = match word, punc with
            | "namespace", "=" ->
              let ns = Namespace.make (parse_word "namespace") in
              Some ns, value_map
            | var_name, "<:" ->
              if check_token '(' then
                let rec parse_values values_rev =
                  let val_start_line, val_start_col = !line, !column in
                  let value = parse_word "value" in
                  let val_pos = pos val_start_line val_start_col in
                  let values_rev' = (val_pos, value)::values_rev in
                  skip_ignorable_tokens ();
                  if peek () =@ c2uni ',' then begin
                    consume ();
                    skip_ignorable_tokens ();
                    parse_values values_rev'
                  end else begin
                    expect_token ")";
                    List.rev values_rev'
                  end in
                let values = parse_values [] in
                let plural = check_token '*' in
                ns_opt,
                (pos start_line start_col, var_name, values, plural)::value_map
              else
                (* Name1 <: Name2; *)
                let referent_name = parse_word "var_name" in
                let referent_value = ListUtil.find_opt
                  (fun (_,name,_,_) -> str_eq name referent_name) value_map in
                (match referent_value with
                  | None ->
                    raise (Failures.Bad_syntax (
                      pos start_line start_col,
                      sprintf "Unknown grammar variable in %s <: %s"
                        var_name referent_name
                    ))
                  | Some (_, _, values, plural) ->
                    let plural' = plural || check_token '*' in
                    (
                      ns_opt,
                      (pos start_line start_col, var_name, values, plural')
                      ::value_map
                    )
                )
            | word, punc ->
              raise (Failures.Bad_syntax (pos start_line start_col,
                "Bad header: " ^ word ^ " " ^ punc)) in
          skip_ignorable_tokens ();
          if peek () =@ c2uni ';' then begin
            consume ();
            skip_ignorable_tokens ();
            if peek () =@ c2uni '}' then begin
              consume ();
              headers'
            end else
              parse_header_body headers_seen' headers'
          end else begin
            expect_token "}";
            headers'
          end in
        parse_header_body [] (None, [])
      end else
        None, [] in
    let ns_opt, var_values_map_rev = parse_headers () in
    let tentative_ns =
      if Namespace.equal ctx.ns_hint Namespace.default then
        (* Use the default namespace for the top-level module.
           A header namespace declaration is only advisory for
           when a grammar is imported into another. *)
        Namespace.default
      else
        match ns_opt with
          | Some ns_hint -> ns_hint
          | None         -> ctx.ns_hint in
    (* Choose a namespace that does not conflict. *)
    let taken_namespaces = Hashtbl.fold (fun _ ns nses -> ns::nses)
      ctx.loaded Namespace.builtin_namespaces in
    let rec choose_ns suffix =
      let candidate, next = match suffix with
        | None   -> tentative_ns, Some 1
        | Some i ->
          Namespace.make
            (Printf.sprintf "%s_%d"
              (Namespace.to_string ns_hint) i),
          Some (i+1) in
      if List.exists (Namespace.equal candidate) taken_namespaces then
        choose_ns next
      else begin
        (* Prevent reuse of the namespace by an import.  We do this early so
           that cyclic imports terminate. *)
        Hashtbl.replace ctx.loaded ctx.load_path candidate;
        candidate
      end in
    let ns = choose_ns None in
    {
      Grammar.namespace = ns;
      grammar_variables = Var.Decls.make (
        List.rev_map
          (fun (pos, name, symbols, plural) ->
            let name = Var.Name.make (Identifier.make ns name) in
            let positional_symbol_list = List.map
              (fun (p, v) -> Some (p, Var.Symbol.make v))
              symbols in
            let domain =
              if plural then
                Var.Domain.Many positional_symbol_list
              else
                Var.Domain.One  positional_symbol_list in
            pos, name, domain)
          var_values_map_rev
      );
    } in
  (* Now that we know the namespace for the file,
     we can start parsing identifiers. *)

  let ns = headers.Grammar.namespace in

  (* Define a series of recursive descent parsers.  Later we use these to parse
     the body of the input. *)
  let rec parse_grammar () =
    let start_line = !line in
    let start_col = !column in
    let rec parse in_imports prods =
      skip_ignorable_tokens ();
      if is_empty () then
        List.rev prods
      else begin
        let prods', in_imports' =
          if in_imports || peek () =@ c2uni '@' then
            parse_import_directive prods, true
          else
            (parse_production ())::prods, false in
        if not (is_empty ()) then expect_token ";";
        parse in_imports' prods'
      end in
    let prods = parse false [] in
    Grammar (pos start_line start_col, headers, prods)

  and parse_identifier () = Identifier.make ns (parse_word "identifier")

  and parse_identifier_matching kind p =
    let start_line = !line in
    let start_col = !column in
    let local_name = parse_word kind in
    let ident = Identifier.make ns local_name in
    if p ident then
      ident
    else
      raise (Failures.Bad_syntax (
        pos start_line start_col,
        "Expected " ^ kind ^ " not (" ^ String.escaped local_name ^ ")"))

  and parse_var_name start_pos raw_name_opt =
    let raw_name = Opt.unless_f (fun _ -> parse_word "var") raw_name_opt in
    let var_ns, raw_name =
      (* Explicit namespaces allowed in predicates since
         variables are not aliasable. *)
      if peek () =@ c2uni '.' then
        let var_ns = Namespace.make raw_name in
        if Namespace.equal var_ns Namespace.synthetic then
          raise (Failures.Bad_syntax (
            start_pos (),
            Printf.sprintf "Expected namespace not (%s)"
              (String.escaped raw_name)
          ));
        consume ();
        Some var_ns, parse_word "var"
      else
        None,        raw_name in

    let local_var_id = Identifier.make ns raw_name in
    if not (Var.Name.is_name local_var_id) then
      raise (Failures.Bad_syntax (
        start_pos (),
        Printf.sprintf "Expected var not (%s)" (String.escaped raw_name)
      ));
    (match var_ns with
      | Some var_ns ->
        Var.Name.make (Identifier.make var_ns raw_name)
      | None ->
        let local_name = Var.Name.make local_var_id in
        if is_none (Var.Decls.name_meta headers.Grammar.grammar_variables
                      local_name) then begin
          let ub_var_id = Identifier.make Namespace.well_known raw_name in
          let ub_name = Var.Name.make ub_var_id in
          if Var.Names.mem ub_name VarsWellKnown.names then
            ub_name
          else
            local_name
        end else
          local_name
    )

  and parse_pred () =
    (* A Var.Pred.t instance can appear inside an annotation parameter list
       to control when the annotation takes effect. *)
    let rec parse_atom () =
      skip_ignorable_tokens ();
      let c = peek () in
      if c =@ c2uni '(' || c =@ c2uni '!' then begin
        let inverted = c =@ c2uni '!' in
        consume();
        if inverted then begin
          skip_ignorable_tokens ();
          expect_token "(";
        end;
        let result = parse_or () in
        expect_token ")";
        if inverted then
          Var.Pred._not result
        else
          result
      end else begin
        let start_line, start_col = !line, !column in
        let name = parse_var_name (fun _ -> pos start_line start_col) None in
        skip_ignorable_tokens ();
        let inverted, many =
          let c = peek () in
          if c =@ c2uni '=' then begin
            consume ();
            false, false
          end else if c =@ c2uni '!' then begin
            expect_token "!=";
            true, false
          end else if c =@ c2uni '<' then begin
            consume ();
            if peek () =@ c2uni '!' then begin
              consume ();
              true, true
            end else begin
              expect_token ":";
              false, true
            end
          end else begin
            expect_token "=";  (* For error message. *)
            false, false
          end in
        skip_ignorable_tokens ();
        let symbols =
          let parse_symbol () =
            let symbol_id = parse_identifier_matching "symbol"
              (fun id -> Var.Symbol.is_symbol (Identifier.local_name id)) in
            (* Symbols are namespaced by the name of the variable with which
               they are associated so we can drop the namespace here. *)
            Var.Symbol.make (Identifier.local_name symbol_id) in
          if many then begin
            expect_token "(";
            let rec parse sym_set =
              skip_ignorable_tokens ();
              if peek () =@ c2uni ')' then
                sym_set
              else begin
                if not (Var.Symbols.is_empty sym_set) then begin
                  expect_token ",";
                  skip_ignorable_tokens ()
                end;
                let symbol = parse_symbol () in
                parse (Var.Symbols.add symbol sym_set)
              end in
            let symbols = parse Var.Symbols.empty in
            expect_token ")";
            symbols
          end else begin
            Var.Symbols.singleton (parse_symbol ())
          end in
        let pred = Var.Pred.Any (name, symbols) in
        if inverted then Var.Pred._not pred else pred
      end
    and parse_and () =
      let rec parse clauses_rev =
        let clauses_rev = parse_atom ()::clauses_rev in
        skip_ignorable_tokens ();
        if peek () =@ c2uni '&' then begin
          consume ();
          parse clauses_rev
        end else
          Var.Pred._and (List.rev clauses_rev) in
      parse []
    and parse_or () =
      let rec parse clauses_rev =
        let clauses_rev = parse_and ()::clauses_rev in
        skip_ignorable_tokens ();
        if peek () =@ c2uni '|' then begin
          consume ();
          parse clauses_rev
        end else
          Var.Pred._or (List.rev clauses_rev) in
      parse [] in
    parse_or ()

  and parse_production () =
    let start_line = !line in
    let start_col = !column in
    let name = parse_identifier () in
    expect_token ":=";
    scope_name := Some name;
    let body = parse_grammar_body () in
    scope_name := None;
    Production ((pos start_line start_col), name, body)

  and parse_import_directive prods =
    (* Parse a structure like
       @import { "path/to/grammer.g" } { local_name := remote_name; ... } *)
    expect_token "@import";
    expect_token "{";
    (* Find the file path to import. *)
    let path = UnicodeSeq.to_utf8 (UnicodeSeq.of_list
      (List.map
        (fun n -> match n with
          | CharSet (_, chars) -> Opt.require (Unicode.Range.Map.min chars)
          | _                  -> failwith "expected CharSet")
        (parse_quoted_string ()))) in
    expect_token "}";
    if ctx.import_depth > 64 then
      raise (Failures.Cannot_load
        (path, "Exceeded import depth.  Maybe there is a cyclic import."));
    expect_token "{";
    (* Figure out which productions in the imported grammar are used under what
       names in the local grammar. *)
    let rec parse_aliases import_ns aliases =
      if check_token '}' then
        List.rev aliases
      else begin
        let alias_line, alias_column = !line, !column in
        let local_name = parse_identifier () in
        let local_name_pos = pos alias_line alias_column in
        skip_ignorable_tokens ();
        let imported_name, imported_name_pos =
          if peek () =@ c2uni ':' then
            (expect_token ":="; skip_ignorable_tokens ();
             let imported_line, imported_column = !line, !column in
             parse_word "identifier", pos imported_line imported_column)
          else
            Identifier.local_name local_name, local_name_pos in
        let imported_name = Identifier.make import_ns imported_name in
        let alias = Production (
          pos alias_line alias_column, local_name,
          Reference (imported_name_pos, imported_name)) in
        let aliases' = alias::aliases in
        if check_token ';' then
          parse_aliases import_ns aliases'
        else begin
          expect_token "}";
          List.rev aliases'
        end
      end in
    (* Open the file and recurse. *)
    let imported_grammar = ctx.grammar_loader
      (callable_once
        (fun canon_path in_channel in_src ->
          match HashtblUtil.maybe_find ctx.loaded (Some canon_path) with
            (* Reuse imports by fetching from the import map. *)
            | Some ns ->
              (* Return a stub.
                 We can't rely on the actual content of a grammar since,
                 when parsing cyclic imports the grammar is not finished by the
                 time it is re-imported. *)
              Grammar (
                in_src,
                { Grammar.namespace = ns; grammar_variables = Var.Decls.empty },
                [])
            | None ->
              let ns_hint' =
                Namespace.make (ImportPrefixHeuristic.choose path) in
              let ctx' = {
                ctx with ns_hint      = ns_hint';
                         import_depth = ctx.import_depth + 1;
                         load_path    = Some canon_path;
              } in
              match parse ctx' true in_channel in_src with
                | G g -> g  (* ok since true passed for as_grammar *)
                | _   -> failwith "expected grammar"))
      (Path.of_string path) in
    ctx.imports := imported_grammar::!(ctx.imports);
    let Grammar (_, imported_headers, _) = imported_grammar in
    let aliases = parse_aliases imported_headers.G.namespace [] in
    List.rev_append aliases prods

  and parse_reference () =
    let start_line = !line in
    let start_col = !column in
    let name = parse_identifier () in
    Reference (pos start_line start_col, name)

  and parse_grammar_body () =
    skip_ignorable_tokens ();
    let start_line = !line in
    let start_col = !column in
    let rec parse_union parts =
      let new_parts = (parse_concatenation [])::parts in
      skip_ignorable_tokens ();
      if peek () =@ c2uni '|' then begin
        let start_line = !line in
        let start_col = !column in
        consume ();
        if peek () =@ c2uni '|' then begin
          (* Grammar authors might mis-type "||" based on muscle memory from
             C/Java like languages.  Treat that as an error instead of
             as a union including the empty string. *)
          consume();
          raise (Failures.Bad_syntax
                   (pos start_line start_col,
                    "(a||b) is illegal.  Use (a|b) or (a | () | b)"))
        end;
        parse_union new_parts
      end
      else
        List.rev new_parts in
    match parse_union [] with
      | [part] -> part
      | parts -> Union ((pos start_line start_col), G.Ordering.Ordered, parts)

  and parse_annotation_params param_name = begin
    let rec parse_comma_list index params_rev =
      let c = peek () in
      let value =
        let start_line = !line in
        let start_col = !column in
        let negate e = match e with
          | Var.Expr.Nin [x] -> x
          | _                -> Var.Expr.Nin [e] in
        AnnotationParam.(match AnnotationMakers.param_kind param_name index with
          | BodyKind ->
            if (is_digit c) || c =@ (c2uni '-') then
              Int (pos start_line start_col, parse_integer ())
            else
              Body (parse_grammar_body ())
          | PredKind ->
            let predicate = parse_pred () in
            let predicate_pos = pos start_line start_col in
            Pred (predicate_pos, predicate)
          | ValueExprKind -> begin
            let rec parse_expression_atom () =
              if check_token '(' then begin
                if check_token ')' then
                  Var.Expr.Val (Var.Value.Many Var.Symbols.empty)
                else begin
                  let e = parse_expression () in
                  expect_token ")";
                  e
                end
              end else if check_token '~' then begin
                negate (parse_expression_atom ())
              end else begin
                let start_line, start_col = !line, !column in
                let word = parse_word "symbol" in
                if Var.Symbol.is_symbol word && peek () <@> c2uni '.' then
                  (* We will convert singular values into plural ones
                     after we know the left-hand-side's name and plurality. *)
                  Var.Expr.Val (Var.Value.One (Var.Symbol.make word))
                else
                  Var.Expr.Ref (
                    parse_var_name
                      (fun _ -> pos start_line start_col) (Some word)
                  )
              end
            and parse_expression_and () =
              match extend '&' parse_expression_atom [] with
                | [x] -> x
                (* a & b & c  ==  ~(~(a & b & c)) *)
                | ls  -> Var.Expr.Nin [Var.Expr.Nin ls]
            and parse_expression_or () =
              match extend '|' parse_expression_and [] with
                | [x] -> x
                (* a | b | c  ==  ~(~a & ~b & ~c) *)
                | ls  -> Var.Expr.Nin (List.map negate ls)
            and extend tok parse exprs_rev = match exprs_rev with
              | [] -> extend tok parse [parse ()]
              | _  ->
                if check_token tok then begin
                  extend tok parse ((parse ())::exprs_rev)
                end else
                  List.rev exprs_rev
            and parse_expression _ = parse_expression_or () in

            let expr_line, expr_column = !line, !column in
            let expression = parse_expression () in
            Expr (pos expr_line expr_column, expression)
          end
        ) in
      let params_rev = value::params_rev in
      if check_token ',' then begin
        skip_ignorable_tokens ();
        parse_comma_list (index + 1) params_rev
      end else
        List.rev (maybe_parse_predicate params_rev)
    and maybe_parse_predicate params_rev =
      if peek () =@ c2uni ':' then begin
        consume ();
        skip_ignorable_tokens ();
        let start_line, start_col = !line, !column in
        let predicate = parse_pred () in
        let predicate_pos = pos start_line start_col in
        AnnotationParam.Pred (predicate_pos, predicate)::params_rev
      end else
        params_rev in
    if peek () =@ c2uni ':' then
      List.rev (maybe_parse_predicate [])
    else
      parse_comma_list 0 []
  end
  and parse_partial_annotation () =
    let start_line = !line in
    let start_col = !column in
    expect_token "@";
    let kind = parse_word "annotation type" in
    skip_ignorable_tokens ();
    let params = (
      if peek () =@ c2uni '{' then begin
        consume ();
        skip_ignorable_tokens ();
        if peek () =@ c2uni '}' then begin
          consume ();
          []
        end else begin
          let params = parse_annotation_params kind in
          expect_token "}";
          params
        end
      end else begin
        []
      end
    ) in
    (start_line, start_col, kind, params)

  and parse_partial_annotations partials =
    skip_ignorable_tokens();
    let c = peek () in
    if c =@ c2uni '@' then
      parse_partial_annotations ((parse_partial_annotation ())::partials)
    else
      partials

  and parse_infix left =
    if peek () =@ c2uni '-' then begin
      consume ();
      let subtrahend = parse_atom () in
      let diff = Difference (body_pos left, left, subtrahend) in
      skip_ignorable_tokens ();
      parse_infix diff
    end else
      left

  and parse_suffix arg =
    let ch = peek () in
    if ch =@ c2uni '+' then begin
      consume ();
      Repetition (body_pos arg, arg)
    end else if ch =@ c2uni '*' then begin
      (* x+ is syntactic sugar for (part+ | ) *)
      consume();
      optional (Repetition (body_pos arg, arg))
    end else if ch =@ c2uni '?' then begin
      (* x? is syntactic sugar for (x | ()) *)
      consume();
      optional arg
    end else arg

  and make_annotation partials body =
    let rec annotate partials body =
      match partials with
        | [] -> body
        | (start_line, start_col, name, params)::rest ->
          let a_pos = pos start_line start_col in
          annotate rest
            (AnnotationMakers.make_annotation headers a_pos name params body) in
    annotate partials body

  and parse_concatenation nodes =
    let partial_annotations = parse_partial_annotations [] in
    (* Prefix operators are part of atom *)
    match maybe_parse_atom () with
      | Some atom ->
        skip_ignorable_tokens ();
        (* Annotations are higher precedence than concatenation and union
           but lower precedence than suffix operators or the difference
           operator *)
        let node = make_annotation
          partial_annotations
          (parse_suffix (parse_infix atom)) in
        parse_concatenation (node::nodes)
      | None -> (
        match partial_annotations with
          | [] -> (
            match (List.rev nodes) with
              | [] -> Concatenation (cursor_pos (), [])
              | [first] -> first
              | (first::_::_) as rnodes ->
                Concatenation (body_pos first, rnodes))
          | _ ->
            raise (Failures.Bad_syntax
                     (cursor_pos (),
                      if is_empty () then
                        "Expected grammar atom not end of input"
                      else
                        sprintf "Expected grammar atom not `%c`"
                          (uni2c (peek ())))))

  and parse_atom () = match maybe_parse_atom () with
    | Some atom -> atom
    | None ->
      raise (Failures.Bad_syntax (cursor_pos (), "Expected grammar atom"))

  and maybe_parse_atom () =
    skip_ignorable_tokens ();
    let c = peek () in
    if c =@ c2uni '"' || c =@ c2uni '\'' then
      Some (parse_quoted_string_as_body ())
    else if c =@ c2uni '(' then (
      consume ();
      let atom = parse_grammar_body ()
      in expect_token ")";
      Some atom)
    else if c =@ c2uni '[' then
      Some (parse_char_set ())
    else if c =@ c2uni '!' then begin
      let start_line = !line in
      let start_col  = !column in
      consume ();
      (* Desugar !x to
         (@Scope{NEG_LA} (@Set{NEG_LA,fail} x | @Set{NEG_LA,pass})
          @If{NEG_LA = pass} ()) *)

      let synth_id = Identifier.make synthetic_ns in

      (* This desugaring is namespace-safe because the only identifiers
         reachable from atom that can be in the synthetic namespace are created
         using the same counter as var_name. *)
      let var_name = synth_id (sprintf "NEG_LA_%d" (ctx.id_counter ())) in
      let var_name = Var.Name.make var_name in

      let atom = parse_atom () in
      let m = pos start_line start_col in
      let end_m = cursor_pos () in
      Some (
        desugar_negative_lookahead
          var_name (fun _ -> m) (fun _ -> end_m) atom
      )
    end
    else if is_identifier_start c then Some (parse_reference ())
    else None

  and parse_quoted_string () =
    let start_line = !line in
    let start_col = !column in
    let delim = peek () in
    if delim =@ c2uni '\'' then
      consume ()
    else
      expect_token "\"";
    let rec parse_chars previous =
      let ch_start_line = !line in
      let ch_start_col = !column in
      let c = peek () in
      if c =@ delim then begin
        consume ();
        List.rev previous
      end else
        let codepoint = match uni2i c with
          | -1 | 0xa | 0xd ->
            raise (Failures.Bad_syntax
                   (pos start_line start_col, "Unclosed string"))
          | 0x5c (* \\ *) -> parse_escape_sequence ()
          | _             -> consume (); c in
        let ranges = Range.Set.singleton codepoint in
        let charset = CharSet (pos ch_start_line ch_start_col, ranges) in
        parse_chars (charset::previous) in
    parse_chars []

  and parse_quoted_string_as_body () =
    let start_line = !line in
    let start_col = !column in
    match parse_quoted_string () with
      | [one_char] -> one_char
      | chars      -> Concatenation (pos start_line start_col, chars)

  and parse_char () =
    let c = peek () in
    match uni2i c with
      | -1 | 0xa | 0xd ->
        fail_if_empty "Expected `]`";
        raise (Failures.Bad_syntax (cursor_pos (), "Newline in [...] charset"))
      | 0x5c -> parse_escape_sequence ()
      | _ -> consume (); c

  and parse_escape_sequence () =
    let start_line = !line in
    let start_col = !column in
    expect_token "\\";
    fail_if_empty "Unfinished escape sequence";
    let c = peek () in
    consume ();
    if   c =@ c2uni '"' || c =@ c2uni '\'' || c =@ c2uni '-'
      || c =@ c2uni '[' || c =@ c2uni '\\' || c =@ c2uni ']'
      || c =@ c2uni '^' then
      c
    else if c =@ c2uni 'b' then c2uni '\b'
    else if c =@ c2uni 't' then c2uni '\t'
    else if c =@ c2uni 'n' then c2uni '\n'
    else if c =@ c2uni 'f' then c2uni '\x0c'
    else if c =@ c2uni 'r' then c2uni '\r'
    else if c =@ c2uni 'x' then i2uni (parse_hex 2 0)
    else if c =@ c2uni 'u' then i2uni (parse_hex 4 0)
    else if c =@ c2uni 'U' then
      let n = parse_hex 8 0 in
      if 0 <= n && n <= uni2i Unicode.max_codepoint then
        match hex_digit_value (peek ()) with
          | None   -> i2uni n
          | Some _ ->
            (* \U00010FFFF is easily misrecognized by a human as a single
               escape sequence.  More easily than 2 and 4 hex digits.
               Fail with an error if we see an ambiguous 9 digit sequence on
               input. *)
            raise (Failures.Bad_syntax (
              pos start_line start_col,
              sprintf "Nine-th hex digit after \\U..."
            ))
      else
        raise (Failures.Bad_syntax (pos start_line start_col,
               sprintf "Bad codepoint U+%x" n))
    else
      raise (Failures.Bad_syntax (pos start_line start_col,
             "Malformed escape sequence"))

  and parse_hex n_digits n =
    if n_digits = 0 then
      n
    else
      let c = peek () in
      let digit_value = match hex_digit_value c with
        | None ->
          fail_if_empty "Expected hex digit";
          raise (Failures.Bad_syntax
                   (cursor_pos (),
                    "Expected hex digit not `" ^ (Utf8.encode c) ^"`"))
        | Some v -> v
      in
      consume ();
      let np = ((n lsl 4) lor digit_value) in
      if np >= n then
        parse_hex (n_digits - 1) np
      else
        raise (Failures.Bad_syntax (cursor_pos (), "Underflow"))

  and hex_digit_value c =
    if (c2uni '0') <=@ c && c <=@ (c2uni '9') then
      Some ((uni2i c) - (int_of_char '0'))
    else if (c2uni 'A') <=@ c && c <=@ (c2uni 'F') then
      Some ((uni2i c) - (int_of_char 'A') + 10)
    else if (c2uni 'a') <=@ c && c <=@ (c2uni 'f') then
      Some ((uni2i c) - (int_of_char 'a') + 10)
    else
      None

  and parse_char_set () =
    let start_line = !line in
    let start_col = !column in
    expect_token "[";
    let inverted = peek () =@ c2uni '^' in
    if inverted then consume ();
    let rec parse_ranges ranges =
      let c = peek () in
      if c =@ c2uni ']' then List.rev ranges
      else
        if c =@ c2uni '[' then
          parse_ranges ((parse_unicode_group ()) @ ranges)
        else
          parse_ranges ((parse_char_range ())::ranges) in
    let ranges = parse_ranges [] in
    expect_token "]";
    let cr = CharSet (pos start_line start_col, Range.Set.make ranges) in
    if inverted then
      (* [^...] is syntactic sugar for char-[...] *)
      let start = SourcePosition.make
        source !scope_name start_line start_col start_line start_col in
      Difference (start, Reference (start, Identifier.make ns "char"), cr)
    else
      cr

  and parse_char_range () =
    let start_line = !line in
    let start_col = !column in
    let left = parse_char () in
    let right =
      (if peek () =@ c2uni '-' then begin
        consume ();
        let c = peek () in
        if c =@ Uni.eof then
          raise (Failures.Bad_syntax
                 (pos start_line start_col,
                  "Expected end of range but got end of input"))
        else if c =@ c2uni ']' then
          raise (Failures.Bad_syntax
                 (pos start_line start_col,
                  "Expected end of range but got `]`"))
        else
          parse_char ()
      end else
        left) in
    if right <@ left then
      raise (Failures.Bad_syntax
                 (pos start_line start_col,
                  sprintf "Bad range U+%x > U+%x" (uni2i left) (uni2i right)));
    Range.make_incl left right

  and parse_unicode_group () =
    let start_line = !line in
    let start_col = !column in
    expect_token "[:";
    let name_chars = UnicodeSeq.of_list (List.rev (consume_while (
      fun cp cps ->
        if cp =@ c2uni ':' then
          None
        else
          Some (cp::cps)) [])) in
    let category_name = UnicodeSeq.to_utf8 name_chars in
    expect_token ":]";
    match UnicodeCategories.get category_name with
      | Some rangeset -> Range.Set.map (fun s e -> Range.make s e) rangeset
      | None ->
        raise (Failures.Bad_syntax (
          pos start_line start_col,
          "Unrecognized unicode category: [:" ^ category_name ^ ":]")) in

  if as_grammar then
    G (parse_grammar ())
  else
    N (parse_grammar_body ())

let fail_on_load _ path =
  raise (Failures.Cannot_load
      (Path.to_string path, "Parser not configured to load"))

let default_grammar_loader = fail_on_load

let make_context grammar_loader = {
  grammar_loader;
  ns_hint        = Namespace.default;
  import_depth   = 0;
  loaded         = Hashtbl.create 16;
  load_path      = None;
  id_counter     = (
    let id = ref 0 in
    fun () ->
      let i = !id in
      if i = min_int then failwith "Overflow";
      incr id;
      i
  );
  imports        = ref [];
}

let parse_grammar ?(grammar_loader=fail_on_load) inp pos =
  let ctx = make_context grammar_loader in
  match parse ctx true inp pos with
    | G (Grammar (m, headers, prods)) ->
      (* Fold other grammars' productions and headers into this grammar. *)
      let prods_lists_rev, headers = List.fold_left
        (fun (prods_lists_rev, headers) (Grammar (_, imp_headers, imp_prods)) ->
          imp_prods::prods_lists_rev,
          {
            headers with G.grammar_variables = (
              Var.Decls.union
                headers.G.grammar_variables
                imp_headers.G.grammar_variables
            );
          })
        ([prods], headers) (List.rev !(ctx.imports)) in
      Grammar (m, headers, List.concat (List.rev prods_lists_rev))
    | _   -> raise (Invalid_argument "not grammar")

let parse_grammar_body inp pos =
  let ctx = make_context fail_on_load in
  match parse ctx false inp pos with
    | N n -> n
    | _   -> raise (Invalid_argument "not node")
