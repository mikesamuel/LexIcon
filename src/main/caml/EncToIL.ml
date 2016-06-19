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

module G    = Grammar
module CUK  = CodeUnitKind
module CUKS = CodeUnitKinds
module SCV  = ScalarCharValue
let sprintf = Printf.sprintf


module Token = struct
  type 'm t =
    | Partial of 'm Regex.t
    | Whole   of 'm Regex.t

  let regex x = match x with | Partial re | Whole re -> re

  let compare a b = match a, b with
    | Partial x, Partial y -> Regex.compare x y
    | Partial _, _         -> ~-1
    | _,         Partial _ -> 1
    | Whole   x, Whole   y -> Regex.compare x y

  let stringer out x = match x with
    | Partial x -> Stringer.ctor "Partial" Regex.stringer out x
    | Whole   y -> Stringer.ctor "Whole"   Regex.stringer out y
end


module type S = sig
  module R : Grammar.Reporting

  module DebugHooks : sig
    module NodeId : Id.S
    module NodeIdMap : MapUtil.S with type key = NodeId.t

    module rec Data : sig
      type t =
        | Nul
        | Fls
        | Tru
        | Num
        | Ind of NodeId.t
        | Rec of NodeId.t
        | Chr of CodeUnit.Range.Set.t
        | Cu  of t
        | Elt of t
        | Key of t
        | Val of t
        | Str of t
        | Arr of t
        | Rel of t
        | Exc of t
        | Or  of DataSet.t
        | Cat of t list

      val compare : t Cmp.t
      val equal : t -> t -> bool
      val stringer : t Stringer.t

      val compact_stringer : (NodeId.t -> Data.t option) -> t Stringer.t
    end and DataSet : SetUtil.S with type elt = Data.t

    type token = R.meta_t Token.t

    type 'a hook = 'a Grammar.grammar -> unit

    type t = {
      encodes : (R.meta_t * NodeId.t * Data.t option)                hook;
      reaches : (R.meta_t * NodeId.t * Data.t option)                hook;
      tokens  : (R.meta_t * NodeId.t * token  option * token option) hook;
      pruned  : (R.meta_t * NodeId.t * bool)                         hook;
      incrs   : (R.meta_t * NodeId.t * bool)                         hook;
      gencode : R.meta_t IL.program -> unit;
      failing : R.meta_t IL.program -> unit;
      checkpt : string -> unit;
      fg_dot  : (out_channel -> unit) -> unit;
      sr_dbg  : SnapshotRecover.DebugHooks.t;
    }

    val default : t
  end

  val enc_to_il :
       ?debug:DebugHooks.t
    -> R.meta_t Grammar.grammar
    -> R.meta_t Grammar.Start.t
    -> Identifier.t list
    -> R.meta_t Enc.t

end



module NodeId : sig
  include Id.S
  val counter : unit -> unit -> t
end = struct
  type t = int

  let compare = cmp_int
  let equal = (=)
  let hash x = x
  let stringer = Stringer.int

  let counter _ = begin
    let r = ref 0 in
    fun _ ->
      let i : t = !r in
      if i < 0 then failwith "underflow";
      incr r;
      i
  end
end

module NodeIdMap = MapUtil.Make (NodeId)
module NodeIdSet = SetUtil.Make (NodeId)
module NodeIdPairMap = MapUtil.Make (struct
  type t = NodeId.t * NodeId.t

  let compare (a, b) (c, d) = Cmp.chain
    (NodeId.compare a c) (lazy (NodeId.compare b d))

  let stringer = Stringer.tup2 NodeId.stringer NodeId.stringer
end)


(* Character set transforms *)
let uni2cu u = CodeUnit.of_int (Unicode.uni2i u)

let unis2cus ranges = begin
  CodeUnit.Range.Set.make
    (Unicode.Range.Map.map
       (fun lt rt () -> CodeUnit.Range.make
         (uni2cu lt) (uni2cu rt))
       ranges)
end

let cus2open_ranges = begin
  let as_point x = IL.Point (CodeUnit.as_int x) in
  fun cus ->
    IL.OpenRange.Set.make (
      CodeUnit.Range.Set.map
        (fun lt rt -> IL.OpenRange.make (as_point lt) (as_point rt))
        cus
    )
end


(* Static handling of variable predicates with Goal=enc, etc. *)
let statically_evaluate_predicate, simplify_predicate = begin
  let knowns = ToolKind.knowns `Enc in
  let partially_evaluate_predicate p = Var.Pred.simplify_f p
    (fun name -> Var.Map.find_opt name knowns) in
  (fun p -> fst (partially_evaluate_predicate p)),
  (fun p -> snd (partially_evaluate_predicate p))
end


let tokenify_grammar
    g start_opt get_meta get_token set_token evaluate_predicates is_live =
begin
  let Grammar.Grammar (_, _, prods) = g in
  let tokenified = ref Identifier.Set.empty in
  let eval_predicate =
    if evaluate_predicates then
      statically_evaluate_predicate
    else
      fun _ -> None
  in
  let rec tokenify_prod visited (G.Production (_, nm, body)) = begin
    let visited' = Identifier.Set.add nm visited in
    let rec tokenify body =
      let token =
        if is_live body then begin match body with
          (* Don't propagate the regex since we need to do a positive
             lookahead assertion at the end, check the predicate, or
             operate on the data input. *)
          | G.Annotation    (_, G.Data         _, b)
          | G.Annotation    (_, G.Set          _, b)
          | G.Annotation    (_, G.Until        _, b) ->
            ignore (tokenify b); None
          | G.CharSet       (m, s) ->
            Some (Token.Partial (Regex.CharSet (get_meta m, unis2cus s)))
          | G.Concatenation (m, ls) ->
            List.iter (fun x -> ignore (tokenify x)) ls;
            let token_rev_opt = List.fold_left
              (fun token_rev_opt el -> Opt.map2
                (fun tok token_rev -> (Token.regex tok)::token_rev)
                (get_token el) token_rev_opt)
              (Some []) ls in
            Opt.map
              (fun ls_rev -> Token.Partial (
                Regex.Concatenation (get_meta m, List.rev ls_rev)))
              token_rev_opt
          | G.Annotation (_, G.Embedded (inner, p), outer) ->
            (match eval_predicate p with
              | Some false -> tokenify outer
              | _          ->
                ignore (tokenify outer);
                ignore (tokenify inner);
                None
            )
          | G.Annotation (m, G.Scope _, b) ->
            (match GrammarParser.resugar_negative_lookahead b with
              | Some lookahead_body ->
                Opt.map
                  (fun t -> Token.Partial (
                    Regex.NegLookahead (get_meta m, Token.regex t)))
                  (tokenify lookahead_body)
              | None                ->
                ignore (tokenify b);
                None
            );
          | G.Annotation (_, G.Entrust      (_, _, p), b) ->
            let token = tokenify b in
            (match eval_predicate p with
              | Some false -> token
              | _          -> None
            )
          | G.Annotation (_, G.Denormalized (x, p),    b) ->
            (match x with
              | Some alt -> ignore (tokenify alt);
              | None     -> ());
            let token = tokenify b in
            (match eval_predicate p with
              | Some false -> token
              | _          -> None
            )
          | G.Annotation (_, G.If           p,         b) ->
            let token = tokenify b in
            (match eval_predicate p with
              | Some true  -> token
              | _          -> None
            )
          | G.Reference (_, name) ->
            tokenify_prod visited' (G.prod_with_name g name)
          | G.Repetition (m, b) ->
            Opt.map
              (fun t ->
                Token.Partial (Regex.Repetition (get_meta m, Token.regex t)))
              (tokenify b)
          | G.Union (m, _, ls) ->
            let ls = List.filter is_live ls in
            List.iter (fun x -> ignore (tokenify x)) ls;
            let rec union token_rev options = match options with
              | []     ->
                Some (
                  Token.Partial (Regex.Union (get_meta m, List.rev token_rev))
                )
              | hd::tl -> (match get_token hd with
                  | Some t -> union ((Token.regex t)::token_rev) tl
                  | None   -> None) in
            union [] ls
          | G.Difference (n, m, s) ->
            let to_simple_regex x = Opt.map
              (fun t -> Regex.simplify (Token.regex t))
              (tokenify x) in
            (match to_simple_regex m, to_simple_regex s with
              | (Some (Regex.CharSet (_, minuend)),
                 Some (Regex.CharSet (_, subtrahend))) ->
                let delta = CodeUnit.Range.Set.difference
                  minuend subtrahend in
                Some (Token.Partial (Regex.CharSet (get_meta n, delta)))
              | Some minuend, Some subtrahend ->
                Some (Token.Partial (
                  Regex.Concatenation (get_meta n, [
                    Regex.NegLookahead (Regex.meta subtrahend, subtrahend);
                    minuend
                  ])
                ))
              | _ -> None
            )
          | G.Panic      _ -> None
          | G.Annotation (_, G.CaseFold _, _)
          | G.Annotation (_, G.Override _, _) -> failwith "not simplified"
        end else None in
      set_token body token;
      token in
    if Identifier.Set.mem nm visited then
      None
    else if Identifier.Set.mem nm !tokenified then
      get_token body
    else begin
      tokenified := Identifier.Set.add nm !tokenified;
      tokenify body
    end
  end in
  List.iter
    (fun (G.Production (_, nm, _) as p) ->
      if not (Identifier.Set.mem nm !tokenified) then
        ignore (tokenify_prod Identifier.Set.empty p);
    )
    prods;

  (* Now, find regular expression roots by walking from the start.

     We ensure that no production body is a token root -- instead, pull
     root-ness through references so that references that paths in the
     flow graph that reach a production body from different places can
     be consistent about whether a node is a token root.
     For example, in
       start := @ValueFalse ([.] t) | @ValueTrue (t);
       t := "-";
     if we marked t a token root then there would be a large regex
     ([.] t) that includes a token root.
  *)
  let rec find_roots visited n = match get_token n with
    | None -> (match n with
        | G.Reference (_, nm) ->
          if not (Identifier.Set.mem nm visited) then
            find_roots (Identifier.Set.add nm visited)
              (G.body_with_name g nm);
        | _ ->
          let rec recurse _ c = match c with
            | G.N child -> find_roots visited child
            | _         -> G.fold recurse () c in
          G.fold recurse () (G.N n)
    )
    | Some (Token.Partial re)
    | Some (Token.Whole   re) ->
      set_token n (Some (Token.Whole (Regex.simplify re)))
  in

  match start_opt with
    | Some start ->
      find_roots Identifier.Set.empty (G.Start.to_body g start)
    | None       -> ()
end
(** Try to collapse sub-trees to regular expressions. *)




module rec Data : sig
  type t =
    | Nul
    | Fls
    | Tru
    | Num
    | Ind of NodeId.t
    (** An indirect reference *)
    | Rec of NodeId.t
    (** [Rec x] is like [Ind x] but is known to be a recursive reference. *)
    | Chr of CodeUnit.Range.Set.t
    | Cu  of t
    | Elt of t
    | Key of t
    | Val of t
    | Str of t
    | Arr of t
    | Rel of t
    | Exc of t
    (** A possible exceptional failure.  A variable value test can cause a
        branch to fail and backtrack to a later branch, so does not reliably
        encode all data encodable by that branch. *)
    | Or  of DataSet.t
    | Cat of t list
  (** A node in a network of data containment relationships which let us
      make a best effort to reason inductively about subset relationships
      between branches in the grammar and hopefully eliminate branches that are
      unreachable because all the data values they could encode are encoded by
      earlier branches.

      For example, in

      {[  "\\u" \@ScalarCharValue (hex hex hex hex)
        | "\\x" \@ScalarCharValue (hex hex)]}

      all the values that could be encoded by the second branch are encoded
      by the first, so we don't need to generate code for the second but in

      {[  "\\x" \@ScalarCharValue (hex hex)
        | "\\u" \@ScalarCharValue (hex hex hex hex)]}

      only some of the values in the second branch are encoded by the first.
      The second option might generate larger code but smaller output.
  *)

  val compare : t Cmp.t

  val equal : t -> t -> bool

  val simplify : (NodeId.t -> Data.t option) -> Data.t -> Data.t

  val is_subset :
       (NodeId.t -> Data.t option) -> Data.t
    -> (NodeId.t -> Data.t option) -> Data.t
    -> bool

  val stringer : t Stringer.t

  val compact_stringer : (NodeId.t -> t option) -> t Stringer.t

end = struct
  type t =
    | Nul
    | Fls
    | Tru
    | Num
    | Ind of NodeId.t
    | Rec of NodeId.t
    | Chr of CodeUnit.Range.Set.t
    | Cu  of t
    | Elt of t
    | Key of t
    | Val of t
    | Str of t
    | Arr of t
    | Rel of t
    | Exc of t
    | Or  of DataSet.t
    | Cat of t list

  let rec compare x y = match x, y with
    | Nul,   Nul   -> 0
    | Nul,   _     -> ~-1
    | _,     Nul   -> 1
    | Fls,   Fls   -> 0
    | Fls,   _     -> ~-1
    | _,     Fls   -> 1
    | Tru,   Tru   -> 0
    | Tru,   _     -> ~-1
    | _,     Tru   -> 1
    | Num,   Num   -> 0
    | Num,   _     -> ~-1
    | _,     Num   -> 1
    | Ind x, Ind y -> NodeId.compare x y
    | Ind _, _     -> ~-1
    | _,     Ind _ -> 1
    | Rec x, Rec y -> NodeId.compare x y
    | Rec _, _     -> ~-1
    | _,     Rec _ -> 1
    | Chr x, Chr y -> CodeUnit.Range.Set.compare x y
    | Chr _, _     -> ~-1
    | _,     Chr _ -> 1
    | Cu  x, Cu  y -> compare x y
    | Cu  _, _     -> ~-1
    | _,     Cu  _ -> 1
    | Elt x, Elt y -> compare x y
    | Elt _, _     -> ~-1
    | _,     Elt _ -> 1
    | Key x, Key y -> compare x y
    | Key _, _     -> ~-1
    | _,     Key _ -> 1
    | Val x, Val y -> compare x y
    | Val _, _     -> ~-1
    | _,     Val _ -> 1
    | Str x, Str y -> compare x y
    | Str _, _     -> ~-1
    | _,     Str _ -> 1
    | Arr x, Arr y -> compare x y
    | Arr _, _     -> ~-1
    | _,     Arr _ -> 1
    | Rel x, Rel y -> compare x y
    | Rel _, _     -> ~-1
    | _,     Rel _ -> 1
    | Exc x, Exc y -> compare x y
    | Exc _, _     -> ~-1
    | _,     Exc _ -> 1
    | Or  x, Or  y -> DataSet.compare x y
    | Or  _, _     -> ~-1
    | _,     Or  _ -> 1
    | Cat x, Cat y -> ListUtil.compare compare x y

  let equal a b = 0 = compare a b

  let rec stringer out x = match x with
    | Nul   -> out "Nul"
    | Fls   -> out "Fls"
    | Tru   -> out "Tru"
    | Num   -> out "Num"
    | Ind x -> Stringer.ctor "Ind" NodeId.stringer          out x
    | Rec x -> Stringer.ctor "Rec" NodeId.stringer          out x
    | Chr x -> out (CodeUnit.ranges_to_string x)
    | Cu  x -> Stringer.ctor "Cu"  stringer                 out x
    | Elt x -> Stringer.ctor "Elt" stringer                 out x
    | Key x -> Stringer.ctor "Key" stringer                 out x
    | Val x -> Stringer.ctor "Val" stringer                 out x
    | Str x -> Stringer.ctor "Str" stringer                 out x
    | Arr x -> Stringer.ctor "Arr" stringer                 out x
    | Rel x -> Stringer.ctor "Rel" stringer                 out x
    | Exc x -> Stringer.ctor "Exc" stringer                 out x
    | Or  x -> Stringer.ctor "Or"  DataSet.stringer         out x
    | Cat x -> Stringer.ctor "Cat" (Stringer.list stringer) out x

  let simplify id_to_data_opt = begin
    let rec simpler visited x = begin match x with
      | Nul | Fls | Tru | Num | Chr _ -> x
      | Ind i ->
        if NodeIdSet.mem i visited then
          Rec i
        else
          (match id_to_data_opt i with
            | Some r -> (match simpler (NodeIdSet.add i visited) r with
                | Ind _ | Rec _ -> Rec i
                | r'            -> r'
            )
            | None    -> x
          )
      | Rec _ -> x
      | Cu  x -> Cu  (simpler visited x)
      | Elt x -> Elt (simpler visited x)
      | Key x -> Key (simpler visited x)
      | Val x -> Val (simpler visited x)
      | Str x -> Str (simpler visited x)
      | Arr x -> Arr (simpler visited x)
      | Rel x -> Rel (simpler visited x)
      | Exc x -> Exc (simpler visited x)
      | Or  s ->
        let s' =
          let rec fold_flat s x = DataSet.fold
            (fun e (s', c') -> match simpler visited e with
              | Or  t -> fold_flat t (s', c')
              | Chr c -> s', CodeUnit.Range.Set.union c c'
              | e'    -> DataSet.add   e' s', c')
          s x in
          let s', c' = fold_flat s (DataSet.empty, CodeUnit.Range.Set.empty) in
          if CodeUnit.Range.Set.is_empty c' then
            s'
          else
            DataSet.add (Chr c') s' in
        if 1 = DataSet.cardinal s' then
          DataSet.min_elt s'
        else
          Or s'
      | Cat ls ->
        let rec cat simple_rev ls = match ls with
          | [] -> (match List.rev simple_rev with
              | [x]    -> x
              | simple -> Cat simple
          )
          | hd::tl -> (match simpler visited hd with
              | Or s when DataSet.is_empty s -> Or s  (*failure is contagious*)
              | Cat simple -> cat (List.rev_append simple simple_rev) tl
              | hd'        -> cat (hd'::simple_rev)                   tl
          ) in
        cat [] ls
    end in
    simpler NodeIdSet.empty
  end

  let fold f x d = match d with
    | Nul | Fls | Tru | Num -> x
    | Ind _ | Rec _ | Chr _ -> x
    | Cu  c | Elt c | Key c
    | Val c | Str c | Arr c
    | Rel c | Exc c         -> f x c
    | Or  a                 -> DataSet.fold (fun e x -> f x e) a x
    | Cat a                 -> List.fold_left f x a

  type prec = TopPrec | OrPrec | CatPrec | UnyPrec
  let compare_prec a b = match a, b with
    | TopPrec, TopPrec -> 0
    | TopPrec, _       -> ~-1
    | _,       TopPrec -> 1
    | OrPrec,  OrPrec  -> 0
    | OrPrec,  _       -> ~-1
    | _,       OrPrec  -> 1
    | CatPrec, CatPrec -> 0
    | CatPrec, _       -> ~-1
    | _,       CatPrec -> 1
    | UnyPrec, UnyPrec -> 0

  let compact_stringer id_to_data out x =
    (* Find the back-references used. *)
    let rec enumerate_back_refs s d = match d with
      | Ind i | Rec i -> NodeIdSet.add i s
      | _             -> fold enumerate_back_refs s d in
    let back_refs = NodeIdSet.fold
      (fun id m -> match id_to_data id with
        | Some d -> DataMap.multiadd NodeIdSet.empty NodeIdSet.add d id m
        | None   -> m)
      (enumerate_back_refs NodeIdSet.empty x) DataMap.empty in
    let nb _ = out Stringer.no_break in
    let paren p q f =
      if compare_prec p q >= 0 then begin
        out "("; f TopPrec; out ")"
      end else
        f q in
    let rec stringify_body prec x = match x with
      | Nul   -> out "Nul"
      | Fls   -> out "Fls"
      | Tru   -> out "Tru"
      | Num   -> out "Num"
      | Ind i -> out "#"; nb (); NodeId.stringer out i
      | Rec i -> out "#"; nb (); NodeId.stringer out i
      | Chr s -> out (CodeUnit.ranges_to_string s)
      | Cu  d -> paren prec UnyPrec (fun p -> out "c:"; nb (); stringify p d)
      | Elt d -> paren prec UnyPrec (fun p -> out "e:"; nb (); stringify p d)
      | Key d -> paren prec UnyPrec (fun p -> out "k:"; nb (); stringify p d)
      | Val d -> paren prec UnyPrec (fun p -> out "v:"; nb (); stringify p d)
      | Str d -> bracket "``" "''" d
      | Arr d -> bracket "["  "]"  d
      | Rel d -> bracket "{"  "}"  d
      | Exc d -> paren prec UnyPrec (fun p -> out "!";  nb (); stringify p d)
      | Or  s -> (match DataSet.cardinal s with
          | 0 -> out "_"
          | 1 ->
            out "(*"; out "singleton_Or"; out ":"; stringify TopPrec x; out "*)"
          | 2 when DataSet.mem (Cat []) s ->
            let optional_data = DataSet.min_elt (DataSet.remove (Cat []) s) in
            paren prec UnyPrec
              (fun p ->
                stringify p optional_data;
                nb (); out "?")
          | _ ->
            paren prec OrPrec
              (fun _ ->
                ignore (
                  DataSet.fold
                    (fun e need_pipe ->
                      if need_pipe then begin
                        nb ();
                        out "|";
                        nb ()
                      end;
                      stringify OrPrec e;
                      true)
                    s false))
      )
      | Cat a -> paren prec CatPrec (fun _ -> List.iter (stringify CatPrec) a)
    and stringify prec x = match DataMap.find_opt x back_refs with
      | Some ids ->
        let parenthesize = match prec with | TopPrec -> false | _ -> true in
        if parenthesize then out "(";
        NodeIdSet.stringer out ids;
        nb ();
        out "->";
        nb ();
        stringify_body TopPrec x;
        if parenthesize then out ")"
      | None -> stringify_body prec x
    and bracket lt rt d =
      out lt;
      nb ();
      stringify TopPrec d;
      nb ();
      out rt
    in
    stringify TopPrec x


  let is_subset x_ind x y_ind y =
    let commitments = ref NodeIdPairMap.empty in

    let rec compare x_seen x y_seen y = match x, y with
      | Ind i, Ind j
      | Rec i, Ind j
      | Ind i, Rec j
      | Rec i, Rec j ->
        let k = (i, j) in
        (match x_ind i, y_ind j with
          | Some x', Some y' ->
            let prior_commitment = NodeIdPairMap.find_opt k !commitments in
            (match prior_commitment with
              | Some b -> b
              | None   ->
                commitments := NodeIdPairMap.add k true !commitments;
                compare (NodeIdSet.add i x_seen) x' (NodeIdSet.add j y_seen) y')
          | _ -> false)
      | Ind i, _
      | Rec i, _
        when not (NodeIdSet.mem i x_seen) -> (match x_ind i with
          | Some x' -> compare (NodeIdSet.add i x_seen) x' y_seen y
          | None    -> false
        )
      | _,     Ind j
      | _,     Rec j ->
        (not (NodeIdSet.mem j y_seen))
        && (match y_ind j with
          | Some y' -> compare x_seen x (NodeIdSet.add j y_seen) y'
          | None    -> false
        )
      | Or  u, _     -> DataSet.for_all (fun o -> compare x_seen o y_seen y) u
      | _,     Or  v -> DataSet.exists (compare x_seen x y_seen) v
      | Ind _, _
      | Rec _, _     -> false
      | Nul,   _
      | Fls,   _
      | Tru,   _
      | Num,   _
      | _,     Nul
      | _,     Fls
      | _,     Tru
      | _,     Num   -> equal x y
      | Cu  a, Cu  b
      | Elt a, Elt b
      | Key a, Key b
      | Val a, Val b
      | Str a, Str b
      | Arr a, Arr b
      | Rel a, Rel b
      | Exc a, Exc b -> compare x_seen a y_seen b
      | Cu  _, _
      | Elt _, _
      | Key _, _
      | Val _, _
      | Str _, _
      | Arr _, _
      | Rel _, _
      | Exc _, _
      | _,     Cu  _
      | _,     Elt _
      | _,     Key _
      | _,     Val _
      | _,     Str _
      | _,     Arr _
      | _,     Rel _
      | _,     Exc _ -> false
      | Chr a, Chr b -> CodeUnit.Range.Set.contains_all b a
      | Chr _, _
      | _,     Chr _ -> false
      | Cat a, Cat b ->
        List.length a = List.length b
        && List.for_all2 (fun x y -> compare x_seen x y_seen y) a b
    in
    compare NodeIdSet.empty x NodeIdSet.empty y

end
and DataMap : (MapUtil.S with type key = Data.t) = MapUtil.Make (Data)
and DataSet : (SetUtil.S with type elt = Data.t) = SetUtil.Make (Data)


module Make (R : G.Reporting) = struct

  module R = R

  let string_for_regex : CUK.t -> 'a Regex.t -> string option = begin
    let space   = CodeUnit.of_int (int_of_char ' ') in
    let upper_a = CodeUnit.of_int (int_of_char 'A') in
    let upper_z = CodeUnit.of_int (int_of_char 'Z') in

    fun cuk re -> begin
      let rec gen cus_rev re = match re with
        | Regex.CharSet (_, s) -> (match CodeUnit.Range.Map.min s with
            | Some cu ->
              let alt_cu =
                if CodeUnit.compare cu space < 0 then
                  space
                else if (CodeUnit.compare upper_a cu <= 0 &&
                           CodeUnit.compare cu upper_z <= 0) then
                  CodeUnit.of_int ((CodeUnit.as_int cu) lor 32)
                else
                  cu in
              let cu = if CodeUnit.Range.Set.has s alt_cu then alt_cu else cu in
              Some (cu::cus_rev)
            | None    -> None)
        | Regex.Concatenation (_, ls) ->
          let rec concat cus_rev ls = match ls with
            | []     -> Some cus_rev
            | hd::tl -> (match gen cus_rev hd with
                | Some cus_rev' -> concat cus_rev' tl
                | None          -> None) in
          concat cus_rev ls
        | Regex.NegLookahead _ ->
          (* Ignore for now but later we try and match the regex which will
             enforce the negative lookahead. *)
          Some cus_rev
        | Regex.Repetition (_, b) -> gen cus_rev b
        | Regex.Union (_, ls) ->
          (* Prefer the empty string if we can get away with it. *)
          if List.exists (Regex.equal (Regex.Concatenation ((), []))) ls then
            Some cus_rev
          else
            let rec try_options ls = match ls with
              | [] -> None
              | hd::tl -> (match gen cus_rev hd with
                  | Some _ as o -> o
                  | None        -> try_options tl) in
            try_options ls
      in
      (match gen [] re with
        | None -> None
        | Some cus_rev ->
          let cus = Array.of_list (List.rev cus_rev) in
          (* Double check that the string actually matches the regex.
             Negative lookaheads like
               (![a]) [a]
             can foil regular expression as can the greedy semantics of loops
             as in
               [ab]+ [b]
          *)
          let reader = {
            Regex.
            is_empty = (fun (_, lt, rt) -> lt = rt);
            read     = (fun (o, lt, rt) -> cus.(lt), (o,lt,lt+1), (o,lt+1,rt));
            stringer = Stringer.tup3 Stringer.int Stringer.int Stringer.int;
            start_of = (fun (o, lt, _)  -> (o, lt, lt));
            compare  = (fun (_, a, _) (_, b, _) -> compare a b);
            join     = (fun ((o0, lt0, rt0) as a) ((_, lt1, rt1) as b) ->
              if      lt1 = rt1 then a
              else if lt0 = rt0 then b
              else                   (o0, lt0, rt1));
            empty    = (0, 0, 0);
          } in
          let start = (0, 0, Array.length cus) in
          Regex.Match.(match (
            Regex.apply_at re reader ~is_eof:true  [start],
            Regex.apply_at re reader ~is_eof:false [start]
           ) with
            | Complete { after=a0; _ }, Complete { after=a1; _ }
            | Complete { after=a0; _ }, Prefix  ({ after=a1; _ }, _)
              when List.for_all reader.Regex.is_empty a0
                && List.for_all reader.Regex.is_empty a1 ->
              let buf = ByteOutput.Buffer.make () in
              Array.iter (fun cu -> CUK.emit cuk cu buf) cus;
              Some (ByteOutput.Buffer.to_string buf)
            | _ -> None
          )
      )
    end
  end
  (** Makes a best effort to find a string that matches a token.
      If it fails, [\@Denormalized] can always be used to explicitly
      provide one. *)


  module DebugHooks = struct
    module NodeId    = NodeId
    module NodeIdMap = NodeIdMap
    module Data      = Data
    module DataSet   = DataSet
    module Token     = Token

    type 'a hook = 'a Grammar.grammar -> unit

    type token = R.meta_t Token.t

    type t = {
      encodes : (R.meta_t * NodeId.t * Data.t option)                hook;
      reaches : (R.meta_t * NodeId.t * Data.t option)                hook;
      tokens  : (R.meta_t * NodeId.t * token  option * token option) hook;
      pruned  : (R.meta_t * NodeId.t * bool)                         hook;
      incrs   : (R.meta_t * NodeId.t * bool)                         hook;
      gencode : R.meta_t IL.program -> unit;
      failing : R.meta_t IL.program -> unit;
      checkpt : string -> unit;
      fg_dot  : (out_channel -> unit) -> unit;
      sr_dbg  : SnapshotRecover.DebugHooks.t;
    }

    let default = {
      encodes = ignore;
      reaches = ignore;
      tokens  = ignore;
      pruned  = ignore;
      incrs   = ignore;
      gencode = ignore;
      failing = ignore;
      checkpt = ignore;
      fg_dot  = ignore;
      sr_dbg  = SnapshotRecover.DebugHooks.default;
    }
  end

  type notes = {
            meta        : R.meta_t;
    (** The original meta-information associated with the node. *)
            id          : NodeId.t;
    (** A unique identifier associated with the node. *)
    mutable live        : bool;
    (** True when the branch might be traversed by the encoder. *)
    mutable encoded     : Data.t option;
    (** The kinds of data encoded by the node.

        This helps us distinguish greedy branches (ones which will never
        be back-tracked out of once returning control to their parent)
        from ones which we need to be able to backtrack out of later.

        Below we do a partial CPT so that we can use the call stack to
        keep track of state we need to recover while backtracking.

        Since CPT is not cheap, being able to prove that later branches
        that encode a subset of what this always encodes can't be reached
        by backtracking allows us to make more branches greedy. *)
    mutable data_reach  : NodeIdSet.t option;
    (** IDs of data encoding nodes that can encode a value immediately before
        control reaches this node. *)
    mutable gen_token   : R.meta_t Token.t option;
    (** Some regex that the node participates in that includes branches.
        By grouping nodes into tokens, we can ignore loops and
        branches that are solely used to choose which token to emit.
    *)
    mutable parse_token : R.meta_t Token.t option;
    (** Like [gen_token] but with dead branches.

        When testing for token merging conflicts we want to avoid conflicts
        between the generated tokens and tokens in the parsed grammar,
        not just tokens in the normalized output grammar. *)
    mutable scv         : SCV.t option;
    (** Used to encode integral values to sequences of digits. *)
    mutable increments  : bool option;
    (** [Some true] for loop nodes that increment a cursor every time their
        body executes, so which don't need to explicitly check the progress
        of their body.  A naive translation of [(x | ())+] might loop
        infinitely since it is always possible to append the empty string to
        an output buffer without exhausting resources. *)
  }
  (** Notes about grammar nodes added to the node. *)


  module Notes = struct
    type meta_t = notes

    let source_pos { meta; _ } = R.source_pos meta

    let join _ = failwith "not joinable"
  end

  module CUKInference = CUKS.Inference (Notes)
  module SCVInference = SCV.Inference  (Notes)

  module Flatten = Flatten.Make (Notes)
  let flatten_grammar (G.Grammar (m, h, ps)) =
    let ps' = List.map
      (fun (G.Production (m, n, b)) -> G.Production (m, n, Flatten.flatten b))
      ps in
    G.Grammar (m, h, ps')


  module FlowG = FlowGraph.Make (Notes) (NodeId)
  let make_flow_graph = FlowG.make
    ~body_to_id:(fun n -> let notes = G.body_meta n in notes.id)
    ~generative:true
    ~partial_eval:simplify_predicate


  let initial_notes id_counter meta = {
    meta;
    id          = id_counter ();
    live        = true;
    encoded     = None;
    data_reach  = None;
    gen_token   = None;
    parse_token = None;
    scv         = None;
    increments  = None;
  }

  module FnKey = struct
    type t = {
      name  : Identifier.t;
      cuks  : CodeUnitKinds.t;
      elide : bool;
    }
    let compare { name=na; cuks=ca; elide=ea } { name=nb; cuks=cb; elide=eb } =
      Cmp.chain
        (Identifier.compare na nb)
        (lazy (Cmp.chain (CUKS.compare ca cb) (lazy (cmp_bool ea eb))))
    let stringer out { name; cuks; elide } = Stringer.rec3
      "name"  Identifier.stringer
      "cuks"  CUKS.stringer
      "elide" Stringer.bool
      out (name, cuks, elide)
  end
  module FnKeyMap = MapUtil.Make (FnKey)
  module FnKeySet = SetUtil.Make (FnKey)


  type fn_vars = {
    fn_idx : Scope.F.Idx.t;
    key    : FnKey.t;
    locals : IL.lscope;
    input  : Scope.L.Idx.t;
    output : Scope.L.Idx.t;
    vars   : Scope.L.Idx.t Var.Map.t;
  }

  let fn_vars_fold2 f x a b = begin
    let x = f x (Some a.input)  (Some b.input) in
    let x = f x (Some a.output) (Some b.output) in
    Var.Map.fold2 (fun _ ai_opt bi_opt x -> f x ai_opt bi_opt) a.vars b.vars x
  end


  (* When translating an annotation to IL statements it is convenient
     to analyze the annotation separately from its body and treat the
     annotation as introducing checks, variables, and actions around
     the statements from the body. *)
  type annot_body_envelope = {
    precondition  : IL.predicate;
    before        : R.meta_t IL.stmt;
    body_vars     : fn_vars;
    after         : R.meta_t IL.stmt;
    postcondition : IL.predicate;
  }


  let enc_to_il ?(debug=DebugHooks.default) g start call_chain = begin
    (* We attach notes to grammar nodes, but those are internal implementation
       details, so save the originals for packaging up into the output once
       we're done. *)
    let original_grammar = g in
    let original_start   = start in

    (* call_chain is currently ignored.
       Accepted for compatibility with Encoder.make *)
    let _ = call_chain in

    (* Don't waste cycles sending stuff to debug_hooks if its just going to be
       ignored. *)
    let maybe_debug
        : 'a . ('a -> unit) -> (unit -> 'a) -> unit
        = fun receiver value_maker -> begin
          if distinct receiver ignore then
            receiver (value_maker ());
        end in
    let checkpt = maybe_debug debug.DebugHooks.checkpt in


    (* Make sure there's a production for us to start at so that we can always
       identify a production that contains each grammar node, and have a 1:1
       correspondance between productions in the input grammar and function in
       the output program. *)
    let start, g = begin
      let start = G.Start.contextualize g start in
      match G.Start.name start with
        | Some _ -> start, g
        | None   ->
          let rec choose_start_prod i =
            let local_name, i' =
              if i = 0 then "start",                      0
              else          "start_" ^ (string_of_int i), i + 1 in
            let candidate = Identifier.make (Identifier.Namespace.synthetic)
              local_name in
            if is_none (G.prod_with_name_opt g candidate) then
              candidate
            else
              choose_start_prod i' in
          let prod_name = choose_start_prod 0 in
          let G.Grammar (m, headers, prods) = g in
          let prod = G.Production (m, prod_name, G.Start.to_body g start) in
          G.Start.named prod_name,
          G.Grammar (m, headers, prod::prods)
    end in
    checkpt (fun _ -> "found start");


    (* Make sure we have a place to collect information about grammar nodes. *)
    let g, start, id_to_notes, id_counter = begin
      let ctr = NodeId.counter () in
      let id_to_notes : notes NodeIdMap.t ref = ref NodeIdMap.empty in
      let allocate_notes x =
        let notes = initial_notes ctr x in
        id_to_notes := NodeIdMap.add notes.id notes !id_to_notes;
        notes in

      let g' = G.grammar_map_meta (fun _ -> allocate_notes) g in
      let start' = G.Start.map_meta allocate_notes start in
      (g', start', !id_to_notes, ctr)
    end in
    let G.Grammar (_, _, prods) = g in
    checkpt (fun _ -> "assigned ids");


    (* Figure out the parse kind and data kind. *)
    let cuks_of : notes Grammar.Start.t -> CUKS.t =
    begin
      let memo_table = ref NodeIdMap.empty in
      fun start ->
        let notes = G.body_meta (G.Start.to_body g start) in
        NodeIdMap.memo (fun _ -> CUKInference.for_grammar g [start]) memo_table
          notes.id
    end in
    checkpt (fun _ -> "inferred cuks");


    (* Walk the grammar to find branches that can be pruned purely based on
       conditions. *)
    let is_live b = (G.body_meta b).live in
    begin
      let check_prod_live (G.Production (_, _, body)) = begin
        (* We iterate until convergence to figure out what's live.
           This is guaranteed to converge since !changed is only true when
           a mutable boolean goes from true to false.
           Since there are a finite numbers of these booleans
           (one per grammar node) the iteration is limited by the count of
           grammar nodes. *)
        let changed = ref false in
        let mark_dead body =
          let body_notes = G.body_meta body in
          if body_notes.live then begin
            body_notes.live <- false;
            changed := true;
          end in
        let rec check_live body =
          let body_notes = G.body_meta body in
          if body_notes.live then begin
            let still_alive = match body with
              | G.CharSet _
              | G.Panic   _ -> true
              | G.Concatenation (_, ls) ->
                List.fold_left (fun live el -> check_live el && live) true ls
              | G.Reference (_, n) -> (G.body_meta (G.body_with_name g n)).live
              | G.Repetition (_, b) -> check_live b
              | G.Union (_, _, ls) ->
                List.fold_left (fun live el -> check_live el || live) false ls
              | G.Difference (_, m, s) -> check_live m && check_live s
              | G.Annotation (_, G.Data _, b) -> check_live b
              | G.Annotation (_, G.Denormalized (None, p), b) ->
                (match statically_evaluate_predicate p with
                  | Some true -> false
                  | _         -> check_live b
                )
              | G.Annotation (_, G.Denormalized (Some alt, p), b) ->
                let alt_live = check_live alt in
                let b_live = check_live b in
                (match statically_evaluate_predicate p with
                  | Some false -> b_live
                  | Some true  -> alt_live
                  | None       -> b_live || alt_live);
              | G.Annotation (_, G.Embedded (inner, p), outer) ->
                let ilive = check_live inner in
                let olive = check_live outer in
                (match statically_evaluate_predicate p with
                  | Some true  -> ilive && olive
                  | None       -> olive
                  | Some false -> mark_dead inner; olive
                )
              | G.Annotation (_, G.Scope   _, b)
              | G.Annotation (_, G.Entrust _, b)
              | G.Annotation (_, G.Set     _, b)
              | G.Annotation (_, G.Until   _, b) -> check_live b;
              | G.Annotation (_, G.If      p, b) ->
                check_live b &&
                (match statically_evaluate_predicate p with
                  | Some x -> x
                  | None   -> true)
              | G.Annotation (_, G.CaseFold _, _)
              | G.Annotation (_, G.Override _, _) -> failwith (
                sprintf "not simplified: %s"
                  (Stringer.s GrammarParser.body_stringer body)) in
            if not still_alive then mark_dead body;
          end;
          body_notes.live in
        ignore (check_live body);
        !changed
      end in
      let rec check_all_prods_live _ =
        (* Enumerate the set of productions reachable from the start, and mark
           dead any production bodies that are not reachable. *)
        let rec try_to_reach reachable n = match n with
          | G.N n when not (is_live n) -> reachable
          | G.N (G.Reference (_, nm)) ->
            if Identifier.Set.mem nm reachable then
              reachable
            else
              try_to_reach (Identifier.Set.add nm reachable)
                (G.N (G.body_with_name g nm))
          | _ -> G.fold try_to_reach reachable n in
        let reachable = try_to_reach Identifier.Set.empty
          (match start with
            | G.Start.Named name -> G.N (G.Reference (G.grammar_meta g, name))
            | G.Start.Body  body -> G.N body) in
        let changed = List.fold_left
          (fun changed (G.Production (_, nm, body) as p) ->
            if Identifier.Set.mem nm reachable then
              check_prod_live p || changed
            else
              let body_notes = G.body_meta body in
              if body_notes.live then begin
                body_notes.live <- false;
                true
              end else
                changed
          )
          false prods in
        if changed then
          check_all_prods_live () in
      check_all_prods_live ();
    end;
    checkpt (fun _ -> "checked liveness");

    let tokenify_grammar =
      tokenify_grammar g (Some start) (fun notes -> notes.meta)
    in
    tokenify_grammar
      (fun b   -> (G.body_meta b).gen_token)
      (fun b t -> (G.body_meta b).gen_token   <- t)
      true
      is_live;
    checkpt (fun _ -> "tokenified gen tokens");
    tokenify_grammar
      (fun b   -> (G.body_meta b).parse_token)
      (fun b t -> (G.body_meta b).parse_token <- t)
      false
      (fun _ -> true);
    checkpt (fun _ -> "tokenified parse tokens");
    (* Mark productions whose bodies are partial dead because they only
       participate in tokens. *)
    List.iter
      (fun (G.Production (_, _, body)) ->
        let notes = G.body_meta body in
        match notes.gen_token with
          | Some (Token.Partial _) -> notes.live <- false
          | _                      -> ()
      )
      prods;
    checkpt (fun _ -> "identified token roots");

    maybe_debug debug.DebugHooks.tokens
      (fun _ -> G.grammar_map_meta
        (fun _ notes ->
          notes.meta, notes.id, notes.gen_token, notes.parse_token)
        g);


    (* Walk the grammar to flesh out notes by tracking the data encoded by
       sub-trees. *)
    begin
      let walk_prod (G.Production (_, _, prod_body)) = begin
        let rec walk_body body =
          let body_notes = G.body_meta body in
          let might_encode = body_notes.live && is_none body_notes.gen_token in
          let encoded = if might_encode then begin
            match body with
              | G.Annotation (_, G.Data t, b) ->
                let notes = G.body_meta b in
                ignore (walk_body b);
                (match t with
                  | POD.CharValue ch ->
                    let cus = match ch with
                      | None -> (match Opt.map Token.regex notes.gen_token with
                          | Some (Regex.CharSet (_, cus)) -> cus
                          | _                             -> failwith (
                            sprintf "expected charset not %s for %s @ %s#%s"
                              (Stringer.s (Stringer.option Token.stringer)
                                 notes.gen_token)
                              (Stringer.s GrammarParser.body_stringer b)
                              (SourcePosition.to_string
                                 (Notes.source_pos body_notes))
                              (Stringer.s NodeId.stringer notes.id)
                          )
                      )
                      | Some cu -> CodeUnit.Range.Set.singleton (uni2cu cu) in
                    Data.Chr cus
                  | POD.Char         -> Data.Cu  (Data.Ind notes.id)
                  | POD.Element      -> Data.Elt (Data.Ind notes.id)
                  | POD.Key          -> Data.Key (Data.Ind notes.id)
                  | POD.KeyValueMap  -> Data.Rel (Data.Ind notes.id)
                  | POD.List         -> Data.Arr (Data.Ind notes.id)
                  | POD.Number       -> Data.Num
                  | POD.ScalarValue base_hint ->
                    let scv = Opt.unless_f
                      (fun _ ->
                        let hint = match base_hint with
                          | None      -> SCV.Unknown
                          | Some base -> SCV.Base base in
                        let scv = SCVInference.infer_from_grammar ~hint
                          g (G.Start.of_body b) in
                        notes.scv <- Some scv;
                        scv)
                      notes.scv in
                      let cus = List.fold_left
                        (fun s seq ->
                          CodeUnit.Range.Set.union s
                            (CodeUnit.Range.Set.single_range
                               seq.SCV.min (seq.SCV.limit)))
                        CodeUnit.Range.Set.empty scv.SCV.sequences in
                      Data.Chr cus
                  | POD.String      -> Data.Str (Data.Ind notes.id)
                  | POD.Value       -> Data.Val (Data.Ind notes.id)
                  | POD.ValueFalse  -> Data.Fls
                  | POD.ValueNull   -> Data.Nul
                  | POD.ValueTrue   -> Data.Tru
                )
              | G.CharSet _
              | G.Panic   _ -> Data.Cat []
              | G.Concatenation (_, ls) ->
                List.iter (fun e -> ignore (walk_body e)) ls;
                Data.Cat (
                  List.map (fun el -> Data.Ind (G.body_meta el).id) ls
                )
              | G.Annotation (_, G.Until        _, b)
              | G.Annotation (_, G.Scope        _, b)
              | G.Annotation (_, G.Entrust      _, b)
              | G.Annotation (_, G.Set          _, b) -> walk_body b
              | G.Annotation (notes, G.Embedded (inner, p), outer) ->
                (* Treat as
                     (@If{p} inner | @If{!p} outer)
                   so that we can use the union branch below to combine the
                   the encoded parts of both branches.
                *)
                (* TODO: Reusing the notes as the meta-information for
                   fabricated nodes is dodgy. *)
                let proxy = G.Union (notes, G.Ordering.Ordered, [
                  G.Annotation (notes, G.If p,                   inner);
                  G.Annotation (notes, G.If (Var.Pred.Nand [p]), outer);
                ]) in
                walk_body proxy
              | G.Annotation (_, G.Denormalized (_, p), b) ->
                let e = walk_body b in
                (match statically_evaluate_predicate p with
                  | Some false -> e
                  | _          -> Data.Exc e
                )
              | G.Annotation (_, G.If           p,      b) ->
                let e = walk_body b in
                (match statically_evaluate_predicate p with
                  | Some true  -> e
                  | _          -> Data.Exc e
                )
              | G.Reference (_, name) ->
                Data.Ind (G.body_meta (G.body_with_name g name)).id
              | G.Repetition (m, b) ->
                ignore (walk_body b);
                let notes = G.body_meta b in
                Data.Cat [
                  Data.Ind notes.id;
                  Data.Or  (DataSet.of_list [Data.Rec m.id; Data.Cat []])
                ]
              | G.Union (_, _, ls) ->
                Data.Or (
                  List.fold_left
                    (fun data el -> DataSet.add (walk_body el) data)
                    DataSet.empty
                    (List.filter is_live ls)
                )
              | G.Annotation (_, G.CaseFold _, _)
              | G.Annotation (_, G.Override _, _)
              | G.Difference _ -> failwith "not simplified"
          end else Data.Cat [] in
          body_notes.encoded <- Some encoded;
          encoded in
        ignore (walk_body prod_body)
      end in
      List.iter walk_prod prods;

      (* Now that we have created the linkages, go back and simplify them. *)
      let unsimplified : Data.t option NodeIdMap.t =
        NodeIdMap.map (fun notes -> notes.encoded) id_to_notes in

      let simplify_data =
        Data.simplify (fun id -> NodeIdMap.find_def id None unsimplified) in

      NodeIdMap.iter
        (fun id notes -> match notes.encoded with
          | Some _ -> notes.encoded <- Some (simplify_data (Data.Ind id))
          | None   -> ())
        id_to_notes;
    end;
    checkpt (fun _ -> "inferred encoded data");
    (* Resolve references against encoded. *)
    let id_to_encoded id = begin match NodeIdMap.find_opt id id_to_notes with
      | Some notes -> notes.encoded
      | None       -> None
    end in

    maybe_debug debug.DebugHooks.encodes
      (fun _ -> G.grammar_map_meta
        (fun _ notes -> notes.meta, notes.id, notes.encoded) g);


    let is_incrementing_data pod = POD.(match pod with
      | Char | Element | Value -> true
      (* Key does not increment. *)
      | Key | String | KeyValueMap | List
      | ValueFalse | ValueTrue | ValueNull
      | CharValue _ | ScalarValue _ | Number -> false
    ) in
    let is_new_cursor_data pod = POD.(match pod with
      | String | KeyValueMap | List -> true
      | Char | Element | Value | Key
      | ValueFalse | ValueTrue | ValueNull
      | CharValue _ | ScalarValue _ | Number -> false
    ) in
    checkpt (fun _ -> "inferred incrementing");


    let fg = make_flow_graph
      ~grammar:g
      ~starts:[start]
      ~pseudo_meta:(fun n -> { (G.meta n) with id=id_counter () }) in
    checkpt (fun _ -> "made flow graph");

    maybe_debug debug.DebugHooks.fg_dot
      (fun _ o -> FlowG.DotOutput.output_graph o fg);

    (* Compute the set of nodes which can consume a data-event prior to each
       data-event consuming node. *)
    begin
      (* We walk the flow graph from each data encoding node and follow paths
         looking for other data encoding nodes that
         1. are reached via a path that fails the original
         2. do not pass through any data encoding nodes that are not also
            failed
         3. are not in the outer envelope of an embedded language that does not
            also include the destination.
      *)
      let data_nodes = FlowG.fold_nodes
        (fun data_nodes node -> match FlowG.Node.body node with
          | G.Annotation (_, G.Data _, _) -> FlowG.NodeSet.add node data_nodes
          | _                             -> data_nodes)
        FlowG.NodeSet.empty fg in

      let contained_by = begin
        let pod_of node = match FlowG.Node.body node with
          | G.Annotation (_, G.Data d, _) -> Some d
          | _                             -> None in
        fun data_node -> match pod_of data_node with
          | None       -> fun _ -> false
          | Some inner -> (fun possible_container ->
            match pod_of possible_container with
              | None       -> false
              | Some outer -> POD.contained_by inner outer
          )
      end in

      (* Avoid O(n**2) behavior of reachability computation. *)
      let path_len_limit = 32 in

      (* Walk paths from each data node keeping track of the data
         nodes on the path which have not been exited by an edge
         with flavor = Fail so that we know when a node is reachable from node
         in the same state that node would be seen in.

         We also keep a set of nodes (data or otherwise) visited to avoid
         cycles in the flow graph.
      *)
      FlowG.NodeSet.iter
        (fun data_node ->
          let data_node_id = (G.body_meta (FlowG.Node.body data_node)).id in
          let rec walk_from nest_depth all_nodes_seen path_len live_nodes node =
            if (not (FlowG.NodeSet.mem node all_nodes_seen)
                && path_len < path_len_limit) then begin
              let is_data_node = FlowG.NodeSet.mem node data_nodes in
              let reached = 0 = nest_depth && is_data_node
                 && FlowG.NodeSet.is_empty live_nodes in
              if reached then begin
                let body = FlowG.Node.body node in
                let notes = G.body_meta body in

                notes.data_reach <- Some (
                  NodeIdSet.add data_node_id
                    (Opt.unless NodeIdSet.empty notes.data_reach)
                );
              end;
              (* Update live nodes with the node we are handling if it is a
                 data node.
                 This, combined with the is_empty check above lets us propagate
                 reachability to alternate that act at the same position in the
                 event stream.
              *)
              let live_nodes =
                if is_data_node then
                  FlowG.NodeSet.add node live_nodes
                else
                  live_nodes in
              let nest_depth =
                if contained_by data_node node then nest_depth + 1
                else nest_depth in
              let all_nodes_seen' = FlowG.NodeSet.add node all_nodes_seen in
              let path_len' = path_len + 1 in
              (* Recurse to adjacent nodes when doing so does not exit
                 the (possibly embedded) grammar that contains data_node
                 or its containing data annotation. *)
              FlowG.fold_outbound
                (fun _ { FlowG.Edge.exits; embeds; target; flavor; _ } ->
                  match flavor, reached with
                    | FlowGraph.EdgeFlavor.Passes, true -> ()
                    | _ when not (FlowG.NodeSet.mem target all_nodes_seen') ->
                      let nest_depth = FlowG.NodeSet.fold
                        (fun exited nest_depth ->
                          if contained_by data_node exited then nest_depth - 1
                          else nest_depth)
                        exits nest_depth in
                      let min_nest_depth, nest_depth = List.fold_left
                        FlowGraph.Embed.(fun (min_nest_depth, nest_depth) em ->
                          let nest_depth' = match em with
                            | EnterInner | EnterOuter -> nest_depth + 1
                            | ExitInner  | ExitOuter  -> nest_depth - 1 in
                          min min_nest_depth nest_depth', nest_depth')
                        (nest_depth, nest_depth) embeds in
                      if min_nest_depth >= 0 then
                        let live_nodes' = match flavor with
                          | FlowGraph.EdgeFlavor.Fails  ->
                            FlowG.NodeSet.diff live_nodes exits
                          | FlowGraph.EdgeFlavor.Passes -> live_nodes in
                        walk_from nest_depth all_nodes_seen' path_len'
                          live_nodes' target
                    | _ -> ()
                )
                () fg node;
            end in
          walk_from 0 FlowG.NodeSet.empty 0 (FlowG.NodeSet.singleton data_node)
            data_node)
        data_nodes
    end;
    checkpt (fun _ -> "inferred data reaches");

    maybe_debug debug.DebugHooks.reaches
      (fun _ ->
        let union_encoded ids =
          let u = Data.Or (
            NodeIdSet.fold
              (fun id s -> DataSet.add (Data.Ind id) s)
              ids DataSet.empty) in
          Data.simplify id_to_encoded u in
        G.grammar_map_meta
          (fun _ notes ->
            notes.meta, notes.id, Opt.map union_encoded notes.data_reach
          ) g);


    (* Identify dead branches that only encode values encoded by earlier
       branches. *)
    if false then begin
      let encoded_ind id = (NodeIdMap.find id id_to_notes).encoded in

      let rec walk n = begin
        (match n with
          | G.A _ -> ()
          | _     ->
            let notes = G.meta n in
            (match notes with
              | { live = true; encoded = Some e; data_reach = Some r; _ } ->
                let reach = Data.Or (
                  NodeIdSet.fold
                    (fun id ds -> DataSet.add (Data.Ind id) ds)
                    r DataSet.empty
                ) in
                let reach = Data.simplify encoded_ind reach in
                if Data.is_subset encoded_ind e encoded_ind reach then begin
                  notes.live <- false
                end
              | _ -> ()
          )
        );
        G.fold (fun _ -> walk) () n
      end in
      walk (G.G g)
    end;
    checkpt (fun _ -> "inferred extraneous encoding branches");


    (* Identify potential token merging conflicts. *)
    begin
      (* There are several kinds of problems that deserve special attention:

         1. In C, "\012" means character U+A not the string ("\x00" "12").
            Octal escapes like \12 come in one, two, or three digit and the
            first two could merge with raw octal digits following them.
            The solution is to use the three-digit form when the next
            character to encode is a digit.

         2. In CSS, "\012" means character U+12 while "\0 " is the string
            containing only the NUL character.
            CSS allows \ followed by one to six hex digits to encode a
            Unicode code-point, and any CSS space character following it is
            treated as a non-coding token-break.
            The solution is to emit a space character when the next character
            is a CSS space or hex digit.

         Unfortunately, these can't be handled in user-code without changes
         to the language because there is no lookahead for the character to
         encode.

         To find them, we walk the flow graph starting at variable length
         scalar value nodes, and look for paths that end up at CharValue nodes
         that pass through only tokens that could be zero in length.

         As we go, we accumulate the set of next characters.

         If the set of encoded next characters overlaps the digits or optional
         interstital characters then we have a merge conflict.

         To solve a merge conflict, we need the IL code to use a global variable
         to track the last character path taken, and lookahead to the next
         character to decide whether to use a long form or set a bit to make
         sure an interstial character is emitted.
      *)
      (* TODO: Auto-handle special cases.
         TODO: Detect cases we can't handle and error out. *)
      ();
    end;


    (* Eliminate dead branches. *)
    let g_live, start_live = begin
      (* Walk the tree and replace dead nodes with zero-element unions.
         Then run the flattener to factor dead nodes out of unions and
         propagate failure to concatenations.

         TODO: Do we need to propagate liveness through references to eliminate
         calls to functions that always fail or can that be left to IL.simplify?
      *)
      let prune = G.map_deep
        ~pre:(fun n -> match n with
          | G.N x ->
            let notes = G.body_meta x in
            if notes.live then
              n
            else
              G.N (G.Union (notes, G.Ordering.Ordered, []))
          | _     -> n) in
      let start' = match start with
        | G.Start.Named _ -> start
        | G.Start.Body  b -> (match prune (G.N b) with
            | G.N b' -> G.Start.of_body (Flatten.flatten b')
            | _      -> failwith "not body") in
      let g' = (match prune (G.G g) with
        | G.G g' -> flatten_grammar g'
        | _      -> failwith "not grammar") in
      (g', start')
    end in
    let G.Grammar (g_notes, headers, prods) = g in
    checkpt (fun _ -> "eliminated dead branches");
    maybe_debug debug.DebugHooks.pruned
      (fun _ ->
        G.grammar_map_meta (fun _ notes -> notes.meta, notes.id, notes.live)
        g_live);
    let _ = start_live in


    (* Figure out which loops and function calls definitely make progress by
       incrementing a cursor. *)
    begin
      (* A node increments when all passing paths from its start to
         its end pass through one of the "incrementing" data
         annotations defined above without first entering one of the
         "new cursor" data annotations. *)

      let checked = ref Identifier.Set.empty in

      let rec body_increments body =
        let body_notes = G.body_meta body in
        let increments = body_notes.live && match body with
          | G.Reference (_, nm) -> prod_increments (G.prod_with_name g nm)
          | G.Concatenation (_, ls) ->
            let is = List.map body_increments ls in
            (* A concatenation increments if there exists a member that
               increments *)
            List.exists (fun x -> x) is
          | G.Union (_, _, ls) ->
            let is = List.map body_increments (List.filter is_live ls) in
            (* A union increments if all non-dead branches increment and there
               is a branch that increments.
               The latter clause is the case because we know from the liveness
               check above that the union has a live branch. *)
            List.for_all (fun x -> x) is
          | G.CharSet    _
          | G.Difference _
          | G.Panic      _ -> false
          | G.Repetition (_, b) -> body_increments b
          | G.Annotation (_, a, b) -> (match a with
              | G.Data t ->
                if is_incrementing_data t then
                  true
                else
                  body_increments b && not (is_new_cursor_data t)
              | G.If _ | G.Scope _ | G.Set _ | G.CaseFold _ | G.Override _
              | G.Until _ | G.Entrust _ | G.Denormalized _ ->
                body_increments b
              | G.Embedded (inner, p) ->
                (match statically_evaluate_predicate p with
                  | Some false -> body_increments b
                  | Some true ->
                    ignore (body_increments b);
                    body_increments inner
                  | None ->
                    let bi = body_increments b in
                    let ii = body_increments inner in
                    bi && ii);
          )
        in
        body_notes.increments <- Some increments;
        increments
      and prod_increments (G.Production (_, nm, body)) =
        if Identifier.Set.mem nm !checked then
          (* This yields false for some recursive productions like
             x := x @Char
             but this is conservative. *)
          (match (G.body_meta body).increments with
            | Some x -> x
            | None   -> false)
        else begin
          checked := Identifier.Set.add nm !checked;
          body_increments body
        end in
      List.iter (fun p -> ignore (prod_increments p)) prods;
    end;
    checkpt (fun _ -> "identified loops");
    maybe_debug debug.DebugHooks.incrs
      (fun _ ->
        G.grammar_map_meta
          (fun _ notes ->
            notes.meta, notes.id, Opt.unless false notes.increments)
        g);


    (* Identify backtracking branches and unroll loops that need to backtrack
       into recursive functions or maybe do a CPT. *)
    (* TODO: Implement me *)


    (* Figure out which productions need which variables.

       [Var.Map.find_opt n (Identifier.Map.find_def p Var.Map.empty) var_inputs]
       is [Some Rw.Read_only] when the production named [p] and/or its callees
       reads the value of the variable names [n].
    *)
    let vars_of =
      SignatureInference.vars_for_grammar g (ToolKind.knowns `Enc) in
    let var_inputs = begin
      List.fold_left
        (fun m (G.Production (_, prod_name, _)) ->
          Identifier.Map.add prod_name (vars_of (G.Start.named prod_name)) m)
        Identifier.Map.empty prods
    end in


    (* Walk the language graph and figure out the kinds of data values that
       can reach data encoding nodes.
       This lets us allocate an appropriately typed temporary for a value input
       if necessary. *)
    let fn_key_to_input_type = begin
      let type_map_ref = ref FnKeyMap.empty in
      let rec walk input_type key visited body = begin
        let notes = G.body_meta body in
        if notes.live then
          match body with
            | G.CharSet _ | G.Panic _ -> ()
            | G.Annotation (_, a, b) ->
              (match a with
                | G.Data t ->
                  let data_kind = key.FnKey.cuks.CUKS.data_kind in
                  let input_type' = match t with
                    | POD.String        -> IL.IData (IL.InputCursor_t data_kind)
                    | POD.Char
                    | POD.CharValue   _
                    | POD.ScalarValue _ -> IL.IData (IL.CodeUnit_t data_kind)
                    | POD.Number        -> IL.Top  (* float | int *)
                    | POD.KeyValueMap   -> IL.IData IL.RelCursor_t
                    | POD.Key
                    | POD.Value         -> IL.Top
                    | POD.List          -> IL.IData IL.ArrCursor_t
                    | POD.Element       -> IL.Top
                    | POD.ValueFalse
                    | POD.ValueTrue     -> IL.EData IL.Bool_t
                    | POD.ValueNull     -> IL.EData IL.Null_t
                  in
                  walk input_type' key visited b
                | G.Denormalized (Some _, p) ->
                  let key' = match statically_evaluate_predicate p with
                    | Some true -> { key with FnKey.elide = true }
                    | _         -> key in
                  walk input_type key' visited b
                | G.CaseFold     _
                | G.Denormalized _
                | G.Scope        _
                | G.Set          _
                | G.If           _
                | G.Override     _
                | G.Entrust      _ ->
                  walk input_type key visited b
                | G.Until limit ->
                  walk input_type key visited limit;
                  walk input_type key visited b
                | G.Embedded (inner, _) ->
                  (* Walk the inner grammar using its parse kinds. *)
                  let inner_cuks = cuks_of (G.Start.of_body inner) in
                  walk input_type { key with FnKey.cuks = inner_cuks }
                    visited inner;
                  walk input_type key visited b
              )
            | G.Concatenation (_, ls)
            | G.Union (_, _, ls) ->
              List.iter (walk input_type key visited) ls
            | G.Difference (_, a, b) ->
              walk input_type key visited a;
              walk input_type key visited b
            | G.Repetition (_, b) ->
              walk input_type key visited b
            | G.Reference (_, nm) ->
              let type_map = !type_map_ref in
              let ref_key = { key with FnKey.name = nm } in
              let typ' = match FnKeyMap.find_opt ref_key type_map with
                | None   -> input_type
                | Some t ->
                  if IL.Equal.ltype input_type t then
                    t
                  else
                    IL.Top in
              type_map_ref := FnKeyMap.add ref_key typ' type_map;
              if not (FnKeySet.mem ref_key visited) then
                walk input_type ref_key (FnKeySet.add ref_key visited)
                  (G.body_with_name g nm)
      end in
      let cuks = cuks_of start in
      let start_prod_name = Opt.require (G.Start.name start) in
      let start_key = { FnKey.name = start_prod_name; cuks; elide = false } in
      walk IL.Top start_key FnKeySet.empty
        (G.Reference (g_notes, start_prod_name));
      !type_map_ref
    end in
    checkpt (fun _ -> "inferred fn inputs");


    (* Names for variables in the IL. *)
    let input_label = Label.of_string "inp" in
    let cursor_label = Label.of_string "cur" in
    let output_label = Label.of_string "out" in
    let input_loop_start_label = Label.of_string "inp_loop_start" in
    let chr_label = Label.of_string "chr" in
    let elt_label = Label.of_string "elt" in
    let key_label = Label.of_string "key" in
    let val_label = Label.of_string "val" in
    let output_start_label = Label.of_string "out_start" in
    let output_limit_label = Label.of_string "out_limit" in
    let embedded_label = Label.of_string "embedded" in
    let denorm_label = Label.of_string "denorm" in


    (* For dealing with variables. *)
    let decls = headers.G.grammar_variables in
    let translate_name, translate_pred, translate_var_expr = begin
      let translate_name fn_vars name =
        let idx = Var.Map.find name fn_vars.vars in
        let e = IL.IRef idx in
        match Scope.L.value fn_vars.locals idx with
          | IL.SPtr _ -> IL.Deref e
          | _         -> e in
      let translate_pred fn_vars p = VarToIL.translate_pred decls
         (translate_name fn_vars) (simplify_predicate p) in
      let translate_var_expr fn_vars name =
        let domn = Opt.require (Var.Decls.domain decls name) in
        VarToIL.translate_var_expr domn
          (translate_name fn_vars) in
      translate_name, translate_pred, translate_var_expr
    end in


    (* Allocate a function for each live production. *)
    let functions : R.meta_t IL.fscope = Scope.F.make () in
    let globals = Scope.G.make () in
    let fn_key_to_vars = begin
      FnKeyMap.mapi
        (fun key input_type ->
          let locals = Scope.L.make () in
          (* Generate local variables for each function. *)
          (* This ordering must be consistent with Signature.simple_enc *)
          let output = Scope.L.add locals output_label
            (IL.EData IL.OutputBuffer_t) in
          let input = Scope.L.add locals input_label input_type in
          let vars = Var.Map.mapi
            (fun name rw ->
              let domn = Opt.require (Var.Decls.domain decls name) in
              let etyp = IL.Enum_t (Var.Domain.map_meta ignore domn) in
              let typ = match rw with
                | Rw.Read_only  -> IL.IData etyp
                | Rw.Read_write
                | Rw.Write_only -> IL.SPtr etyp in
              Scope.L.add locals (Label.of_identifier (Var.Name.as_id name)) typ
            )
            (Identifier.Map.find key.FnKey.name var_inputs) in
          let fn_notes = G.body_meta (G.body_with_name g key.FnKey.name) in
          let fn_idx =
            Scope.F.add functions (Label.of_identifier key.FnKey.name) (IL.Fn (
              locals, Scope.L.length locals,
              IL.Cond (fn_notes.meta, IL._false))
            )
          in
          {
            fn_idx;
            key;
            locals;
            input;
            output;
            vars;
          })
        fn_key_to_input_type
    end in
    checkpt (fun _ -> "made fn skeletons");


    (* Build function bodies. *)
    begin
      let block_of meta ls = Associativity.right
        (fun _ -> IL.Cond (meta, IL._true))
        (fun a b -> IL.Block (R.join [IL.Meta.stmt a; IL.Meta.stmt b], a, b))
        ls
      in

      let alt_of meta ls = Associativity.right
        (fun _ -> IL.Cond (meta, IL._false))
        (fun a b -> IL.Alt   (R.join [IL.Meta.stmt a; IL.Meta.stmt b], a, b))
        ls
      in

      let nop meta = IL.Cond (meta, IL._true) in

      let override_fn_idx_map = ref Identifier.Map.empty in

      let append meta fn_vars e = begin
        if fn_vars.key.FnKey.elide then
          nop meta
        else
          IL.Mut (meta, IL.Append (e, fn_vars.output))
      end in

      let rec xlate fn_vars body =
        let { live; gen_token; meta; _ } = G.body_meta body in
        if live then begin
          (match gen_token with
            | None
            | Some (Token.Partial _)   -> xlate_body fn_vars body
            | Some (Token.Whole   tok) ->
              let { CUKS.parse_kind; _ } = fn_vars.key.FnKey.cuks in
              (match string_for_regex parse_kind tok with
                | None         -> IL.Cond (meta, IL._false)
                | Some ""      -> nop meta
                | Some tok_str -> append meta fn_vars (IL.StrLit tok_str))
          )
        end else
          IL.Cond (meta, IL._false)
      and xlate_body fn_vars body = begin match body with
        | G.CharSet (notes, _) ->
          failwith (
            sprintf
              (
                "%s: must be a whole token or reachable only via a whole token"
                ^^ " not %s"
              )
              (SourcePosition.to_string (Notes.source_pos notes))
              (Stringer.s (Stringer.option Token.stringer) notes.gen_token)
          )
        | G.Concatenation (notes, ls)    ->
          block_of notes.meta (List.map (xlate fn_vars) ls)
        | G.Union         (notes, _, ls) ->
          alt_of   notes.meta (List.map (xlate fn_vars) ls)
        | G.Reference (notes, callee) ->
          let callee_key = { fn_vars.key with FnKey.name = callee } in
          let callee_fn_vars = FnKeyMap.find callee_key fn_key_to_vars in
          let callee_idx_to_caller_expr = fn_vars_fold2
            (fun m caller_idx_opt callee_idx_opt ->
              match caller_idx_opt, callee_idx_opt with
                | _, None -> m
                | Some caller_idx, Some callee_idx ->
                  let caller_expr =
                    match Scope.L.value fn_vars.locals caller_idx with
                      | IL.Top | IL.EData _ -> `EE (IL.ERef caller_idx)
                      | IL.IData _ -> `IE (IL.IRef caller_idx)
                      | IL.SPtr  _ ->
                        let callee_type =
                          Scope.L.value callee_fn_vars.locals callee_idx in
                        (match callee_type with
                          | IL.SPtr _ -> `IE (IL.IRef caller_idx)
                          | _         -> `IE (IL.Deref (IL.IRef caller_idx))) in
                  Scope.L.IdxMap.add callee_idx caller_expr m
                | None, _ -> failwith "missing variable"
            )
            Scope.L.IdxMap.empty
            fn_vars callee_fn_vars in
          let actuals = List.map snd
            (Scope.L.IdxMap.bindings callee_idx_to_caller_expr) in
          IL.Call (notes.meta, callee_fn_vars.fn_idx, actuals)
        | G.Repetition (notes, b) ->
          (* Make sure we make progress by capturing the input cursor state. *)
          let loop_start_type_opt =
            let input_type = Scope.L.value fn_vars.locals fn_vars.input in
            (match input_type with
              | IL.IData IL.ArrCursor_t
              | IL.IData IL.RelCursor_t       ->
                Some (IL.IData IL.CursorSnapshot_t)
              | IL.IData (IL.InputCursor_t k) ->
                Some (IL.IData (IL.InputSnapshot_t k))
              | _ -> None) in
          (match loop_start_type_opt with
            | Some loop_start_type ->
              let meta = notes.meta in
              let capture = `IE (IL.Snapshot (IL.IRef fn_vars.input)) in
              let loop_start_idx = Scope.L.add
                fn_vars.locals input_loop_start_label loop_start_type in
              let init, test = match (G.body_meta b).increments with
                | Some true -> nop meta, IL._true
                | _ ->
                  IL.Let (meta, loop_start_idx, capture),
                  IL.Lt (IL.IRef loop_start_idx,
                         IL.Snapshot (IL.IRef fn_vars.input))
              in
              let loop_body = xlate fn_vars b in
              IL.Loop (meta, IL.Block (meta, init, loop_body), test)
            | None ->
              (* Don't repeat. *)
              xlate fn_vars b
          )
        | G.Panic { meta; _ } -> IL.Panic meta
        | G.Difference _ -> failwith "not simplified"
        | G.Annotation (notes, a, b) ->
          let reject msg e =
            failwith (
              sprintf "%s: %s %s"
                (SourcePosition.to_string (Notes.source_pos notes))
                (Stringer.s
                   (IL.SourceStringers.actual globals fn_vars.locals) e)
                msg) in
          let require_ee e = match e with
            | `EE x -> x
            | `IE _ -> reject "not `EE" e
          in
          let require_ie e = match e with
            | `EE _ -> reject "not `IE" e
            | `IE x -> x
          in
          let meta = notes.meta in
          (match a with
            | G.CaseFold     _
            | G.Override     _            -> failwith "not simplified"
            | G.Data         t            ->
              (* The current input *)
              let input_expr =
                match Scope.L.value fn_vars.locals fn_vars.input with
                  | IL.Top
                  | IL.EData _ -> `EE (IL.ERef fn_vars.input)
                  | IL.IData _ -> `IE (IL.IRef fn_vars.input)
                  | IL.SPtr  _ -> `IE (IL.Deref (IL.IRef fn_vars.input)) in
              (* Look at the kind of data and decide whether to translate the
                  body or just use it for inference. *)
              let body_xlate_ref = ref (fun fn_vars' -> xlate fn_vars' b) in
              (* Derives a new local variable as we decompose a complex value
                 into smaller ones. *)
              let check_assign_use_do test typ label new_input_expr after =
                let new_input_idx = Scope.L.add fn_vars.locals label typ in
                {
                  precondition  = test;
                  before        = IL.Let (meta, new_input_idx, new_input_expr);
                  body_vars     = { fn_vars with input = new_input_idx };
                  after;
                  postcondition = IL._true;
                }
              in
              let decompose_input typ sub_input_label sub_input_expr = begin
                let input_iexpr = require_ie input_expr in
                let increment_stmt =
                  if is_incrementing_data t then
                    IL.Mut (meta, IL.Incr (fn_vars.input, IL.IntLit 1, None))
                  else
                    nop meta
                in
                check_assign_use_do
                  (IL._not (IL.Empty input_iexpr))
                  typ sub_input_label sub_input_expr increment_stmt
              end in
              let make_cursor inp_typ cursor_typ =
                let input_expr_typ =
                  IL.typeof globals fn_vars.locals input_expr in
                let test, init, input_expr = match input_expr_typ with
                  | IL.Top ->
                    (* Cast the input via a let *)
                    let cast_label = Label.of_string (match inp_typ with
                      | IL.Array_t         -> "arr"
                      | IL.Relation_t      -> "rel"
                      | IL.InputBuffer_t _ -> "str"
                      | _                  -> "cst") in
                    let cast_idx = Scope.L.add fn_vars.locals cast_label
                      (IL.EData inp_typ) in
                    IL.Is (require_ee input_expr, inp_typ),
                    IL.Let (meta, cast_idx, input_expr),
                    `EE (IL.ERef cast_idx)
                  | x when IL.Equal.ltype x (IL.EData inp_typ) ->
                    IL._true, nop meta, input_expr
                  | _ ->
                    IL.Is (require_ee input_expr, inp_typ), nop meta, input_expr
                in
                let cursor_expr = IL.StartOf (require_ee input_expr) in
                let envelope = check_assign_use_do test
                  (IL.IData cursor_typ) cursor_label (`IE cursor_expr)
                  (nop meta)
                in
                {
                  envelope with
                  before = IL.Block (meta, init, envelope.before);
                  postcondition = IL.Empty (IL.IRef envelope.body_vars.input);
                }
              in
              let check_input precondition = {
                precondition;
                before        = nop meta;
                body_vars     = fn_vars;
                after         = nop meta;
                postcondition = IL._true;
              } in

              (* Swap out the current input variable. *)
              let k = fn_vars.key.FnKey.cuks.CUKS.data_kind in
              let annot_body_envelope = match t with
                | POD.Char               ->
                  decompose_input (IL.IData (IL.CodeUnit_t k))
                    chr_label (`IE (IL.Read (require_ie input_expr)))
                | POD.Element            ->
                  decompose_input IL.Top elt_label
                    (`EE (IL.ElAt  (require_ie input_expr)))
                | POD.Key                ->
                  decompose_input IL.Top key_label
                    (`EE (IL.KeyAt (require_ie input_expr)))
                | POD.Value              ->
                  decompose_input IL.Top val_label
                    (`EE (IL.ValAt (require_ie input_expr)))
                | POD.CharValue   uniopt ->
                  (* Figure out which code-units this annotation can encode. *)
                  let body_notes = G.body_meta b in
                  let cus = match uniopt with
                    | Some u -> CodeUnit.Range.Set.singleton (uni2cu u)
                    | None   -> (match body_notes.gen_token with
                        | Some (Token.Whole (Regex.CharSet (_, cus))) ->
                          (* Don't recurse to the body which will just end up
                             using one code-unit instead of encoding the current
                             value. *)
                          body_xlate_ref := (fun _ ->
                            append meta fn_vars
                              (IL.Cptoa (require_ie input_expr))
                          );
                          cus
                        | _ ->
                          failwith "char_value does not encode a charset")
                  in
                  (* Don't swap the inputs.
                     Just test that the code-unit matches those that are
                     encodable by the body or restricted by the annotation
                     parameter. *)
                  check_input
                    (IL.In (require_ie input_expr, cus2open_ranges cus))
                | POD.KeyValueMap        ->
                  make_cursor IL.Relation_t IL.RelCursor_t
                | POD.List               ->
                  make_cursor IL.Array_t    IL.ArrCursor_t
                | POD.Number             ->
                  body_xlate_ref := IL.(fun _ ->
                    let input_eexpr = require_ee input_expr in
                    let int_branch = Block (
                      meta,
                      Cond (meta, Is (input_eexpr, Int_t)),
                      append meta fn_vars (Itoa input_eexpr)
                    ) in
                    let float_branch = Block (
                      meta,
                      Cond (meta, Is (input_eexpr, Float_t)),
                      append meta fn_vars (Ftoa input_eexpr)
                    ) in
                    Alt (meta, int_branch, float_branch)
                  );
                  check_input IL._true
                | POD.ScalarValue _      ->
                  (* Look at the SCV to figure out the encodable ranges. *)
                  let body_notes = G.body_meta b in
                  let scv = Opt.require body_notes.scv in
                  body_xlate_ref := (fun _ ->
                    append meta fn_vars (IL.Ntoa (require_ie input_expr, scv))
                  );
                  let cus = CodeUnit.Range.Set.make (
                    List.map
                      (fun { SCV.min; limit; _ } ->
                        CodeUnit.Range.make min limit)
                      (scv.SCV.sequences)
                  ) in
                  check_input
                    (IL.In (require_ie input_expr, cus2open_ranges cus))
                | POD.String             ->
                  make_cursor (IL.InputBuffer_t k) (IL.InputCursor_t k)
                | POD.ValueFalse
                | POD.ValueTrue          ->
                  let input_eexpr = require_ee input_expr in
                  let type_test = IL.Is (input_eexpr, IL.Bool_t) in
                  let bool_val = IL.ToPrim (input_eexpr, IL.Bool_t) in
                  let value_test = match t with
                    | POD.ValueFalse -> IL._not (IL.BoolIdent bool_val)
                    | _              -> IL.BoolIdent bool_val in
                  check_input (IL._and [type_test; value_test])
                | POD.ValueNull          ->
                  check_input (IL.Is (require_ee input_expr, IL.Null_t))
              in
              block_of meta [
                IL.Cond (meta, annot_body_envelope.precondition);
                annot_body_envelope.before;
                (!body_xlate_ref) annot_body_envelope.body_vars;
                annot_body_envelope.after;
                IL.Cond (meta, annot_body_envelope.postcondition);
              ]
            | G.Denormalized (None, p) ->
              let p' = translate_pred fn_vars p in
              IL.Block (meta, xlate fn_vars b, IL.Cond (meta, IL._not p'))
            | G.Denormalized (Some alt, p) ->
              (match statically_evaluate_predicate p with
                | Some false -> xlate fn_vars b
                | Some true  ->
                  let fn_vars_eliding = {
                    fn_vars with key = {
                      fn_vars.key with FnKey.elide = true
                    }
                  } in
                  IL.Block (meta, xlate fn_vars_eliding b, xlate fn_vars alt)
                | None       ->
                  (* First try the body, but elide content *)
                  let p' = translate_pred fn_vars p in
                  let denorm_idx = Scope.L.add fn_vars.locals denorm_label
                    (IL.IData IL.OutputSnapshot_t) in
                  let body_stmt = xlate fn_vars b in
                  let alt_stmt = xlate fn_vars alt in
                  block_of meta [
                    (* Capture the output buffer location before we write out
                       content that may prove to be denormalized. *)
                    IL.Let (meta, denorm_idx,
                            `IE (IL.EndOf (IL.ERef fn_vars.output)));
                    body_stmt;
                    IL.Alt (
                      meta,
                      (* If the predicate fails, we're done. *)
                      IL.Cond (meta, IL._not p'),
                      IL.Block (
                        meta,
                        (* Otherwise, roll back any denormalized output *)
                        IL.Mut (
                          meta,
                          IL.Truncate (IL.IRef denorm_idx, fn_vars.output)
                        ),
                        (* Then perform the alternative. *)
                        alt_stmt
                      )
                    )
                  ]
              )
            | G.If           p            ->
              IL.Block (
                meta,
                IL.Cond (meta, translate_pred fn_vars p),
                xlate fn_vars b
              )
            | G.Scope        (name, _)    ->
              (match GrammarParser.resugar_negative_lookahead b with
                | Some _ -> nop meta
                | None   ->
                  let domn = Opt.require (Var.Decls.domain decls name) in
                  let etyp = IL.Enum_t (Var.Domain.map_meta ignore domn) in
                  let label = Label.of_identifier (Var.Name.as_id name) in
                  let typ, init = begin
                    (* We need to allocate a pointer instead of
                       a local variable when the variable is mutated by callees.
                    *)
                    let rec is_passed_writable n = match n with
                      | G.N b when not (is_live b) -> false
                      | G.N (G.Annotation (_, G.Scope (nm, _), c)) ->
                        not (Var.Name.equal name nm)
                        && is_passed_writable (G.N c)
                      | G.N (G.Reference (_, callee_name)) ->
                        let vars_passed = Identifier.Map.find_def
                          callee_name Var.Map.empty var_inputs in
                        (match Var.Map.find_opt name vars_passed with
                          | None
                          | Some Rw.Read_only  -> false
                          | Some Rw.Read_write
                          | Some Rw.Write_only -> true)
                      | _ ->
                        G.fold (fun x c -> x || is_passed_writable c) false n in
                    if is_passed_writable (G.N b) then
                      (
                        IL.SPtr  etyp,
                        fun i -> IL.Let (meta, i, `IE (IL.AllocPtr (etyp)))
                      )
                    else
                      (
                        IL.IData etyp,
                        fun _ -> nop meta
                      )
                  end in
                  let var_idx = Scope.L.add fn_vars.locals label typ in
                  let fn_vars' = {
                    fn_vars with vars = Var.Map.add name var_idx fn_vars.vars
                  } in
                  IL.Block (meta, init var_idx, xlate fn_vars' b)
              )
            | G.Set          (name, expr) ->
              let var_idx = Var.Map.find name fn_vars.vars in
              let value_expr = translate_var_expr fn_vars name expr in
              let assignment = match Scope.L.value fn_vars.locals var_idx with
                | IL.IData _ -> IL.Let (meta, var_idx, `IE value_expr)
                | IL.SPtr  _ -> IL.Mut (meta, IL.SetPtr (var_idx, value_expr))
                | _          -> failwith "invalid enum" in
              IL.Block (meta, assignment, xlate fn_vars b)
            | G.Until        _            ->
              (* TODO: Check the limit expression against the encoded data.
                 after the content following the until is encoded. *)
              xlate fn_vars b
            | G.Entrust      (nm, vs, p)  ->
              (match statically_evaluate_predicate p with
                | Some false ->
                  (* Don't allocate override fn *)
                  xlate fn_vars b
                | _ ->
                  (* TODO: Capture the start location, do the work, capture the
                     end location, then call the entrusted function with the
                     input value and output buffer slice. *)
                  let output_start_idx = Scope.L.add fn_vars.locals
                    output_start_label (IL.IData IL.OutputSnapshot_t) in
                  let output_limit_idx = Scope.L.add fn_vars.locals
                    output_limit_label (IL.IData IL.OutputSnapshot_t) in
                  let capture_start = IL.Let (
                    meta,
                    output_start_idx,
                    `IE (IL.EndOf (IL.ERef fn_vars.output))
                  ) in
                  let encode_stmt = xlate fn_vars b in
                  let capture_limit = IL.Let (
                    meta,
                    output_limit_idx,
                    `IE (IL.EndOf (IL.ERef fn_vars.output))
                  ) in
                  let p' = translate_pred fn_vars p in
                  (* Combine the variables explicitly marked readable and the
                     variables set by the body of the annotation to derive
                     the set of variables passed to the @Entrusted fn. *)
                  let vars = Var.Names.fold
                    (fun name m -> Var.Map.add_if_absent name Rw.Read_only m)
                    vs (vars_of (G.Start.of_body b)) in
                  let override_actuals =
                    [
                      (`IE (IL.IRef fn_vars.input));
                      (`IE (IL.IRef fn_vars.output));
                    ] @ (
                      (* Iterate the variables in declaration order. *)
                      List.fold_right
                        (fun name ls -> match Var.Map.find_opt name vars with
                          | Some Rw.Read_only ->
                            (`IE (translate_name fn_vars name))::ls
                          | Some Rw.Read_write
                          | Some Rw.Write_only ->
                            let idx = Var.Map.find name fn_vars.vars in
                            (`IE (IL.IRef idx))::ls
                          | None -> ls
                        )
                        (Var.Decls.names_in_order decls) []
                    ) in
                  let override_fn_idx = Identifier.Map.memo
                    (fun name ->
                      let label = Label.of_identifier name in
                      let actual_types = List.map
                        (IL.typeof globals fn_vars.locals) override_actuals in
                      Scope.F.add functions label
                        (IL.Override (meta, label, actual_types)))
                    override_fn_idx_map nm
                  in
                  block_of meta [
                    capture_start;
                    encode_stmt;
                    capture_limit;
                    IL.Alt (
                      meta,
                      IL.Block (
                        meta,
                        IL.Cond (meta, IL._not p'),
                        IL.Call (meta, override_fn_idx, override_actuals)),
                      IL.Cond (meta, IL._not p')
                    )
                  ]
              )
            | G.Embedded     (inner, p)   ->
              let reencode_stmt _ = begin
                (* We need to know the portion of the output encoded using the
                   inner grammar. *)
                let output_start_idx = Scope.L.add fn_vars.locals
                  output_start_label (IL.IData IL.OutputSnapshot_t) in
                let output_limit_idx = Scope.L.add fn_vars.locals
                  output_limit_label (IL.IData IL.OutputSnapshot_t) in
                (* Encode using the inner grammar *)
                let capture_start = IL.Let (
                  meta,
                  output_start_idx,
                  `IE (IL.EndOf (IL.ERef fn_vars.output))
                ) in
                (* TODO: Use the CUKs for the inner grammar here. *)
                let encode = xlate fn_vars inner in
                let capture_limit = IL.Let (
                  meta,
                  output_limit_idx,
                  `IE (IL.EndOf (IL.ERef fn_vars.output))
                ) in
                (* Grab the encoded portion. *)
                let k = fn_vars.key.FnKey.cuks.CUKS.data_kind in
                let encoded_str_idx = Scope.L.add fn_vars.locals
                  embedded_label (IL.EData (IL.InputBuffer_t k)) in
                let capture = IL.Block (
                  meta,
                  IL.Let (meta, encoded_str_idx, `EE (IL.FreezeBuffer (
                    IL.SliceBuffer (
                      IL.ERef fn_vars.output,
                      IL.IRef output_start_idx, IL.IRef output_limit_idx, k),
                    k)
                  )),
                  IL.Mut (meta, IL.Truncate (
                    IL.IRef output_start_idx, fn_vars.output
                  ))
                ) in
                let fn_vars' = { fn_vars with input = encoded_str_idx } in
                block_of meta [
                  capture_start;
                  encode;
                  capture_limit;
                  capture;
                  xlate fn_vars' b
                ]
              end in
              (* Re-encode as a string using the outer grammar. *)
              (match statically_evaluate_predicate p with
                | Some true  -> reencode_stmt ()
                | Some false -> xlate fn_vars b
                | None       ->
                  let p' = translate_pred fn_vars p in
                  IL.Alt (
                    meta,
                    IL.Block (
                      meta, IL.Cond (meta, p'),         reencode_stmt ()),
                    IL.Block (
                      meta, IL.Cond (meta, IL._not p'), xlate fn_vars b)
                  )
              )
        )
      end in
      FnKeyMap.iter
        (fun fn_key fn_vars ->
          let body' = xlate fn_vars (G.body_with_name g fn_key.FnKey.name) in
          let fn_idx = fn_vars.fn_idx in
          Scope.F.set functions fn_idx
            (match Scope.F.value functions fn_idx with
              | IL.Fn (locals, arity, _) -> IL.Fn (locals, arity, body')
              | f -> f)
        )
        fn_key_to_vars;
    end;
    checkpt (fun _ -> "built fn bodies");


    (* Build an IL program from the grammar. *)
    let program = begin
      let start_key = {
        FnKey.
        name  = Opt.require (G.Start.name start);
        cuks  = cuks_of start;
        elide = false;
      } in
      let start_fn_idx = (FnKeyMap.find start_key fn_key_to_vars).fn_idx in
      ILSimplify.simplify (IL.Program (globals, functions, start_fn_idx))
    end in
    let program = IL.alpha_rename program in
    checkpt (fun _ -> "built program");
    maybe_debug debug.DebugHooks.gencode (fun _ -> program);


    (* Add instructions to the program to reset state mutated on failing paths
       so we can backtrack to alternate branches. *)
    let program = SnapshotRecover.fail_gracefully
      ~debug_hooks:debug.DebugHooks.sr_dbg
      program
    in
    checkpt (fun _ -> "snapshot & recover");
    maybe_debug debug.DebugHooks.failing (fun _ -> IL.alpha_rename program);


    (* Polish the program and package it for delivery. *)
    let program = ILSimplify.simplify program in
    let program = IL.alpha_rename program in
    checkpt (fun _ -> "simplified & renamed");


    {
      Enc.
      grammar = original_grammar;
      start   = original_start;
      program;
      cuks    = cuks_of start;
    }
  end
end


type 'm rfp_notes = {
          original_meta : 'm;
  mutable token         : 'm Token.t option;
}
let regexs_for_productions g = begin
  let Grammar.Grammar (_, _, prods) as annotated_grammar =
    Grammar.grammar_map_meta
      (fun _ original_meta -> { original_meta; token=None })
      g
  in
  tokenify_grammar
    annotated_grammar
    None
    (fun m    -> m.original_meta)
    (fun b    -> (Grammar.body_meta b).token)
    (fun b re -> (Grammar.body_meta b).token <- re)
    false
    (fun _ -> true);
  List.fold_left
    (fun m (Grammar.Production (_, name, body)) ->
      match (Grammar.body_meta body).token with
        | Some (Token.Whole   re)
        | Some (Token.Partial re) ->
          Identifier.Map.add name (Regex.simplify re) m
        | None                    -> m
    )
    Identifier.Map.empty prods
end
