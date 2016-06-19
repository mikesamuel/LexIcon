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

module G = Grammar
module Range = Unicode.Range

module type AnnotatableGrammar = sig
  type meta_t
  type annot_meta_t
  val source_pos : meta_t -> SourcePosition.t
  val annotate : meta_t -> Range.Set.t -> annot_meta_t
  val ranges : annot_meta_t -> Range.Set.t
end

module AnnotatedGrammarPrinter (A : AnnotatableGrammar) = struct
  let rec char_range_printer { Range.lt; Range.rt } =
    if Unicode.equal lt Unicode.eof then
      if Unicode.diff rt lt = 1 then
        "[:eof:]"
      else
        "[:eof:]" ^ char_range_printer (Range.make Unicode.zero rt)
    else
      let esc_char c = String.escaped (Utf8.encode c) in
      match Unicode.diff rt lt with
        | 1 -> esc_char lt
        | 2 -> (esc_char lt) ^ (esc_char (Unicode.sum rt ~-1))
        | _ -> (esc_char lt) ^ "-" ^ (esc_char (Unicode.sum rt ~-1))

  let char_rangeset_printer ranges = Range.Set.to_string
    ~combine:(fun _ ranges -> "[" ^ (String.concat "" ranges) ^ "]")
    ~range_to_string:char_range_printer ranges

  let string_of_annotated_grammar annotated_grammar = Stringer.s
    (fun o x -> GrammarParser.make_grammar_stringer
      ~str_meta:(fun out meta -> out (char_rangeset_printer (A.ranges meta)))
      o x)
    annotated_grammar
end

module Followers (A : AnnotatableGrammar) = struct

  module IdentHashtbl = Hashtbl.Make(Identifier)

  module DEBUG = AnnotatedGrammarPrinter(A)

  let any = Range.Set.single_range_incl Unicode.zero Unicode.max_codepoint

  let followers (G.Grammar (m, headers, prods)) starts =
    let n_prods = List.length prods in

    let in_start = G.Start.referenced_by starts in

    (* Maps production names to the set of characters that might follow
       a use of that production. *)
    let ref_followers = IdentHashtbl.create n_prods in

    (* Maps production names to the set of characters that might precede
       the start of that production. *)
    let prod_preceders = IdentHashtbl.create n_prods in

    (* Assume that all productions are followed and preceded by the
       empty set.  Later we iterate until convergence expanding these sets. *)
    List.iter
      (fun (G.Production (_, name, _)) ->
        IdentHashtbl.replace ref_followers name Range.Set.empty;
        IdentHashtbl.replace prod_preceders name Range.Set.empty)
      prods;

    (* Assume conservatively that the start productions specify at least
       one finite-length string. *)
    List.iter
      (fun (G.Production (_, prod_name, _)) ->
        if Identifier.Set.mem prod_name in_start then
          IdentHashtbl.replace ref_followers prod_name
          (Range.Set.singleton Unicode.eof))
      prods;

    (*
      [annotate_node node followers] annotates a grammar parse-tree with
      a conservative (overly large) set of characters that can immediately
      follow any prefix that occurs before node is entered.
      @param followers the set of characters that can follow node.
      @return the annotated node, the preceder set of node.
     *)
    let rec annotate_node node followers = match node with
      | G.Annotation (m, a, b) ->
        let annotated_body, preceders = annotate_node b followers in
        G.Annotation (
          A.annotate m preceders,
          G.annot_map_meta (fun _ m -> A.annotate m Range.Set.empty) a,
          annotated_body), preceders

      | G.CharSet (m, ranges) ->
        G.CharSet (A.annotate m ranges, ranges), ranges

      | G.Concatenation (m, children) ->
        let annotated_children, preceders =
          let rec children_backwards children =
            (match children with
              | [] -> [], followers
              | child::rest ->
                let annotated_children, followers =
                  children_backwards rest in
                let annotated_child, preceders =
                  annotate_node child followers in
                annotated_child::annotated_children, preceders) in
          children_backwards children in
        (* The set that follows the start of a concatenation is the set that
           precedes the first element. *)
        G.Concatenation (A.annotate m preceders, annotated_children), preceders

      | G.Difference (m, minuend, subtrahend) ->
        (* Ideally this would have been folded to a charset by now. *)
        let annotated_minuend, preceders = annotate_node minuend followers in
        G.Difference (
          A.annotate m preceders,
          annotated_minuend,
          G.body_map_meta (fun _ m -> A.annotate m Range.Set.empty) subtrahend),
        preceders

      | G.Panic m ->
        (* Nothing can follow a panic. *)
        G.Panic (A.annotate m Unicode.Range.Set.empty),
        Unicode.Range.Set.empty

      | G.Reference (m, name) ->
        (* Add the call site followers with those from the map for the
           next call to (annotate_prod name) to consider. *)
        if IdentHashtbl.mem ref_followers name then
          IdentHashtbl.replace
            ref_followers name
            (Range.Set.union
               (IdentHashtbl.find ref_followers name)
               followers);
        let preceders = if IdentHashtbl.mem prod_preceders name then
            IdentHashtbl.find prod_preceders name
          else
            (* If we can't resolve the ref, assume the worst. *)
            any in
        G.Reference (A.annotate m preceders, name),
        preceders

      | G.Repetition (m, body) ->
        (* If the last repetition of body is preceded by P and followed by F,
           then if we treat the last repetition as optional, and consider
           a previous instance of body, then that previous instance can be
           followed by P or F, so we can compute its preceders by treating
           it as followed by (P u F).
           We iterate until convergence.
         *)
        let rec converge annotated_body followers preceders =
          let prev_instance_followers = Range.Set.union followers preceders in
          let prev_annotated_body, prev_instance_preceders =
            annotate_node body prev_instance_followers in
          if (G.Equal.body prev_annotated_body annotated_body
              && Unicode.Range.Set.equal prev_instance_preceders preceders) then
            annotated_body, preceders
          else
            converge prev_annotated_body followers prev_instance_preceders in
          (* Repetitions always match at least once. *)
        let annotated_body, preceders = annotate_node body followers in
        let annotated_body, preceders =
          converge annotated_body followers preceders in
        G.Repetition (A.annotate m preceders, annotated_body), preceders

      | G.Union (m, o, children) ->
        let annotated_children, preceders =
          List.fold_right
            (fun child (annotated_children, preceders) ->
              let annotated_child, child_preceders =
                annotate_node child followers in
              (annotated_child::annotated_children,
               Range.Set.union preceders child_preceders))
            children ([], Range.Set.empty) in
        G.Union (A.annotate m preceders, o, annotated_children), preceders in

    let annotate_prod (G.Production (m, name, body)) =
      let followers = IdentHashtbl.find ref_followers name in
      let annotated_body, preceders = annotate_node body followers in
      IdentHashtbl.replace prod_preceders name preceders;
      G.Production (A.annotate m preceders, name, annotated_body) in

    let annotate_grammar () =
      G.Grammar (
        A.annotate m (Range.Set.empty),
        G.headers_map_meta (fun m -> A.annotate m Range.Set.empty) headers,
        List.map annotate_prod prods) in

    Conv.iter_until_convergence
      ~eq:(Pervasives.(=))  (* We need to consider meta-data. *)
      (fun _ -> annotate_grammar ())
      (annotate_grammar ())

end
