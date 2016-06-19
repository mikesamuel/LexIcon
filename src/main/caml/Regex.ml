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

module Range = CodeUnit.Range

type 'a t =
  | CharSet       of 'a * Range.Set.t
  | Repetition    of 'a * 'a t
  | Concatenation of 'a * 'a t list
  | Union         of 'a * 'a t list
  | NegLookahead  of 'a * 'a t

module Match = struct
  type 'i t =
    | NoMatch
    | Complete     of 'i regions
    | Prefix       of 'i regions * 'i continuation
  and 'i regions = {
    before : 'i list;
    at     : 'i list;
    after  : 'i list;
  }
  and 'i continuation = ?is_eof:bool -> 'i list -> 'i t

  let regions_stringer input_stringer out r = match r with
    | { before = []; after = []; at } -> Stringer.list input_stringer out at
    | { before = []; after; at } ->
      Stringer.rec2
        "at"     (Stringer.list input_stringer)
        "after"  (Stringer.list input_stringer)
        out (after, at)
    | { before; after; at } ->
      Stringer.rec3
        "before" (Stringer.list input_stringer)
        "at"     (Stringer.list input_stringer)
        "after"  (Stringer.list input_stringer)
        out (before, after, at)

  let stringer input_stringer out m = match m with
    | NoMatch             -> out "NoMatch"
    | Complete     c      ->
      Stringer.ctor "Complete" (regions_stringer input_stringer) out c
    | Prefix       (c, _) ->
      Stringer.ctor "Prefix"   (regions_stringer input_stringer) out c
end

let meta x = match x with
  | CharSet       (m, _)
  | Repetition    (m, _)
  | Concatenation (m, _)
  | Union         (m, _)
  | NegLookahead  (m, _) -> m

let rec map_meta f r =
  let mm = map_meta f in
  match r with
    | CharSet       (m, r)  -> CharSet       (f m, r)
    | Repetition    (m, c)  -> Repetition    (f m, mm c)
    | Concatenation (m, ls) -> Concatenation (f m, List.map mm ls)
    | Union         (m, ls) -> Union         (f m, List.map mm ls)
    | NegLookahead  (m, c)  -> NegLookahead  (f m, mm c)

let rec eq_ignore_meta a b = match a, b with
  | CharSet       (_, r), CharSet       (_, s) -> CodeUnit.Range.Set.equal r s
  | CharSet       _,      _                    -> false
  | Repetition    (_, c), Repetition    (_, d) -> eq_ignore_meta c d
  | Repetition    _,      _                    -> false
  | Concatenation (_, c), Concatenation (_, d) ->
    ListUtil.for_all2_soft eq_ignore_meta c d
  | Concatenation _,      _                    -> false
  | Union         (_, c), Union         (_, d) ->
    ListUtil.for_all2_soft eq_ignore_meta c d
  | Union         _,      _                    -> false
  | NegLookahead  (_, c), NegLookahead  (_, d) -> eq_ignore_meta c d
  | NegLookahead  _,      _                    -> false


let cu2u cu = Unicode.i2uni (CodeUnit.as_int cu)

let single_char r =
  if Range.Set.is_singleton r then
    (match Range.Map.min r with
      | Some cu when CodeUnit.as_int cu <= 0x7f ->
        Some (char_of_int (CodeUnit.as_int cu))
      | _ -> None)
  else
    None

let is_ordinary_printable ch =
  ('A' <=% ch && ch <=% 'Z')
    || ('a' <=% ch && ch <=% 'z')
    || ('0' <=% ch && ch <=% '9')
    || ch =% '_'

let rec stringer out r = match r with
  | CharSet       (_, r) ->
    (match single_char r with
      | Some ch when is_ordinary_printable ch ->
        out (String.make 1 ch)
      | _ ->
        let ur = Unicode.Range.Set.make (
          CodeUnit.Range.Set.map
            (fun lt rt -> Unicode.Range.make (cu2u lt) (cu2u rt)) r) in
        let str = Unicode.Range.Set.to_string
          ~combine:(fun _ ranges -> "[" ^ (String.concat "" ranges) ^ "]")
          ~range_to_string:Unicode.Range.esc_range
          ur in
        out str)
  | Repetition    (_, b)
  | Union         (_, [Repetition (_, b); Concatenation (_, [])])
  | Union         (_, [b;                 Concatenation (_, [])]) ->
    let parenthesize =
      match b with | Concatenation (_, _::_) | Union _ -> true | _ -> false in
    if parenthesize then out "(";
    stringer out b;
    if parenthesize then out ")";
    (match r with
      | Union (_, [Repetition _; _]) -> out "*"
      | Union _                      -> out "?"
      | _                            -> out "+");
  | Concatenation (_, []) -> out "("; out ")"
  | Concatenation (_, ls) ->
    List.iter
      (fun child -> match child with
        | Concatenation _ | Union _ -> out "("; stringer out child; out ")"
        | _ -> stringer out child)
      ls
  | Union         (_, []) -> out "fail"
  (* If you have fewer than two options then you have no options. *)
  | Union         (_, [x]) -> out "("; out "/*|*/"; stringer out x; out ")"
  | Union         (_, ls) ->
    ignore (List.fold_left
      (fun needs_bar child ->
        if needs_bar then out "|";
        (match child with
          | Union _ -> out "("; stringer out child; out ")"
          | _ -> stringer out child);
        true)
      false ls)
  | NegLookahead  (_, NegLookahead (_, c)) ->
    out "(?=";
    stringer out c;
    out ")"
  | NegLookahead  (_, c) ->
    out "(?!=";
    stringer out c;
    out ")"

let chars_stringer = Range.Set.stringer

let rec repr_stringer out re = match re with
  | CharSet       (_, r)  -> Stringer.ctor "CharSet"       chars_stringer out r
  | Union         (_, ls) -> Stringer.ctor "Union"         reprs_stringer out ls
  | Concatenation (_, ls) -> Stringer.ctor "Concatenation" reprs_stringer out ls
  | NegLookahead  (_, b)  -> Stringer.ctor "NegLookahead"  repr_stringer  out b
  | Repetition    (_, b)  -> Stringer.ctor "Repetition"    repr_stringer  out b
and reprs_stringer out re = Stringer.list repr_stringer out re

let rec chars_matched re = match re with
  | CharSet       (_, r)  -> r
  | Union         (_, ls)
  | Concatenation (_, ls) ->
    List.fold_left (fun r el -> CodeUnit.Range.Set.union r (chars_matched el))
      CodeUnit.Range.Set.empty ls
  | NegLookahead  _       -> CodeUnit.Range.Set.empty
  | Repetition    (_, b)  -> chars_matched b

type result =
  | AlwaysFails
  | CanBeEmpty
  | IsEmpty
  | NotEmpty

let result_stringer out r =
  out (
    match r with
      | AlwaysFails -> "AlwaysFails"
      | CanBeEmpty  -> "CanBeEmpty"
      | IsEmpty     -> "IsEmpty"
      | NotEmpty    -> "NotEmpty"
  )

let _ = result_stringer

let rec last_empty ls = match ls with
  | []                      -> false
  | [Concatenation (_, [])] -> true
  | _::tl                   -> last_empty tl

let rec simplify re = match re with
  | CharSet       (m, s)    ->
    if Range.Set.is_empty s then
      Union (m, []), AlwaysFails
    else
      re, NotEmpty
  | NegLookahead  (m, body) -> (match simplify body with
    | _, AlwaysFails -> Concatenation (m, []), IsEmpty
    | body', _       -> (match body' with
      | Concatenation (_, [])                    -> Union (m, []), AlwaysFails
      | Union         (_, ls) when last_empty ls -> Union (m, []), AlwaysFails
      (* !!!(x) -> !(x) *)
      | NegLookahead  (_, (NegLookahead _ as x)) -> x,             IsEmpty
      | _                                        ->
        NegLookahead  (m, body'), IsEmpty
    )
  )
  | Concatenation (m, ls)   ->
    let rec simplify_cat ls_rev r ls = match ls with
      | []     ->
        let ls' = List.fold_left
          (fun ls' el -> match el, ls' with
            | NegLookahead (_, b0), NegLookahead (m, b1)::tl ->
              (NegLookahead (m, fst (simplify (Union (m, [b0; b1])))))::tl
            | NegLookahead (_, CharSet (_, neg)), CharSet (m, pos)::tl ->
              CharSet (m, Range.Set.difference pos neg)::tl
            | _ -> el::ls')
          [] ls_rev in
        (match ls' with
          | [x] -> x
          | _   -> Concatenation (m, ls')),
        r
      | hd::tl -> (match simplify hd with
          | hd', AlwaysFails -> hd', AlwaysFails
          | hd', r'          ->
            let all_r' = match r, r' with
              | AlwaysFails, _
              | _,           AlwaysFails -> AlwaysFails
              | IsEmpty,     IsEmpty     -> IsEmpty
              | CanBeEmpty,  IsEmpty
              | IsEmpty,     CanBeEmpty
              | CanBeEmpty,  CanBeEmpty  -> CanBeEmpty
              | NotEmpty,    _
              | _,           NotEmpty    -> NotEmpty in
            let ls_rev' =  match hd' with
              | Concatenation (_, ls') -> List.rev_append ls' ls_rev
              | _                      -> hd'::ls_rev in
            simplify_cat ls_rev' all_r' tl
      ) in
    simplify_cat [] IsEmpty ls
  | Union         (m, ls) ->
    let rec simplify_children ls_rev r ls = match ls with
      | []     ->
        let ls' = List.fold_left
          (fun ls' el -> match ls', el with
            | CharSet (m, r0)::tl, CharSet (_, r1) ->
              CharSet (m, Range.Set.union r0 r1)::tl
            | _ -> el::ls')
          [] ls_rev in
        (match ls' with
          | []  -> Union (m, []),  AlwaysFails
          | [x] -> x,              r
          | _   -> Union (m, ls'), r)
      | hd::tl -> (match simplify hd with
        | _,   AlwaysFails -> simplify_children ls_rev r tl
        | hd', r'          ->
          let all_r' = match r, r' with
            | AlwaysFails, _           -> r'
            | _,           AlwaysFails -> r
            | IsEmpty,     IsEmpty     -> IsEmpty
            | NotEmpty,    NotEmpty    -> NotEmpty
            | CanBeEmpty,  _
            | _,           CanBeEmpty
            | _,           NotEmpty
            | _,           IsEmpty     -> CanBeEmpty in
          (match hd' with
            | Concatenation (_, []) -> simplify_children (hd'::ls_rev) all_r' []
            | Union         (_, ls) ->
              simplify_children (List.rev_append ls ls_rev) all_r' tl
            | _                     -> simplify_children (hd'::ls_rev) all_r' tl
          )
      ) in
    simplify_children [] AlwaysFails ls
  | Repetition    (m, body) ->
    let body', r' = simplify body in

    (* When trying to simplify a repeated union so that options that match the
       empty string are not repeatedly applied, we pull out empty elements which
       must be (due to flattening):
         1. negative lookaheads, or
         2. the empty string
       and then ensure that non-empty elements aren't over-matched so
       as not to change the semantics of constructs like:
       (![y]|[xy])+ which should match the empty string when a string
       starts with "y".

          (![y] | [x] | ![z] | [w])+
       -> (!![y] [x] | (!![y] !![z]) [w])+ | ![y] | ![z]

       Returns
           non_empty_options,  -- modified [x] and [w] above
           empty_options       -- ![y] and ![z] above.
    *)
    let decompose_repeated_union ls =
      let rec decompose rev_non_empty rev_empty ls = match ls with
        | [] -> List.rev rev_non_empty, List.rev rev_empty
        | (Concatenation (_, []) as hd)::_ ->
          decompose rev_non_empty (hd ::rev_empty) []
        | (NegLookahead _ as nla)::tl ->
          decompose rev_non_empty (nla::rev_empty) tl
        | hd::tl ->
          (* ![y] | x -> !![y] [x]
             TODO: add a rule to concat to simplify these unnecessary positive
             lookaheads out. *)
          let guarded_hd = List.rev (
            hd::(List.map (fun x -> NegLookahead (meta x, x)) rev_empty)
          ) in
          let hd', r = simplify (Concatenation (meta hd, guarded_hd)) in
          assert (match r with | NotEmpty -> true | _ -> false);
          decompose (hd'::rev_non_empty) rev_empty tl in
      decompose [] [] ls in

    (match r' with
      | NotEmpty              -> Repetition (m, body'), NotEmpty
      | AlwaysFails | IsEmpty -> body', r'
      | CanBeEmpty            -> (match body' with
          | Union         (m, ls) ->
            let non_empty_options, empty_options =
              decompose_repeated_union ls in
            let simple_rep, r = simplify (
              Union (
                m,
                (Repetition (m, Union (m, non_empty_options)))::empty_options
              )
            ) in
            assert (match r with | CanBeEmpty -> true | _ -> false);
            simple_rep
          | Concatenation (m, ls) ->
            (* Each element must be able to match "" if the whole concatenation
               can match "".
               At least one element must be able to match a non-empty string
               if the whole concatenation must not match "".
               Since elements must be (due to folding) of the form:
               1. Negative lookahead which always match ""
               2. Unions which can match non-empty strings.
               Convert
                   (![x] ![y] ([z] | something_empty) ...)+
               to
                   (![x] ![y] ([z] ... | <...>))+ | ![x] ![y]
               where <...> is ... but recursively processed so that it includes
               a non-empty element.
            *)
            let loop_body_opt,  (* A loop body that will not match "". *)
                empty_options   (* All the branches which matches "". *)
              =
              let decompose_concat ls =
                let rec decompose rev_empty ls = match ls with
                  | []                         ->
                    None, List.rev rev_empty
                  | (NegLookahead _ as hd)::tl ->
                    decompose (hd::rev_empty) tl
                  | (Union (m, opts))     ::tl ->
                    let non_empty_options, empty_options =
                      decompose_repeated_union opts in
                    let non_empty_union, empty_union =
                      Union (m, non_empty_options), Union (m, empty_options) in

                    let one_non_empty_string =
                      Concatenation (m, non_empty_union::tl) in

                    let tl_alternative, tl_empty = decompose [] tl in

                    let non_empty_string = match tl_alternative with
                      | None   -> one_non_empty_string
                      | Some x ->
                        Union (m, [
                          one_non_empty_string;
                          Concatenation (m, [
                            empty_union; NegLookahead (m, non_empty_union); x
                          ]);
                        ]) in

                    Some (
                      Concatenation (m, List.rev (non_empty_string::rev_empty))
                    ),
                    List.rev_append rev_empty (empty_union::tl_empty)
                  | Concatenation _       ::_     (* not flattened *)
                  | CharSet       _       ::_     (* concat cannot be empty *)
                  | Repetition    _       ::_  -> (* repetition not fixed up
                                                     by this branch *)
                    failwith (Printf.sprintf "unexpected %s"
                                (Stringer.s stringer (List.hd ls))) in
                decompose [] ls in
              decompose_concat ls in
            let simple_rep, r = simplify (
              Union (
                m,
                [
                  Repetition    (m, Opt.require loop_body_opt);
                  Concatenation (m, empty_options);
                ]
              )
            ) in
            assert (match r with | CanBeEmpty -> true | _ -> false);
            simple_rep
          | _                 ->
            failwith (Printf.sprintf "%s cannot be empty"
                        (Stringer.s stringer body'))
      ), CanBeEmpty
    )

let simplify re = fst (simplify re)

let rec equal a b = match a, b with
  | CharSet       (_, x), CharSet       (_, y) -> CodeUnit.Range.Set.equal x y
  | Repetition    (_, x), Repetition    (_, y)
  | NegLookahead  (_, x), NegLookahead  (_, y) -> equal x y
  | Concatenation (_, x), Concatenation (_, y)
  | Union         (_, x), Union         (_, y) -> ListUtil.equal equal x y
  | CharSet       _,      _
  | Repetition    _,      _
  | NegLookahead  _,      _
  | Concatenation _,      _
  | Union         _,      _                    -> false

let rec compare a b = match a, b with
  | CharSet       (_, x), CharSet       (_, y) -> CodeUnit.Range.Set.compare x y
  | CharSet       _,      _                    -> ~-1
  | _,                    CharSet       _      -> 1
  | Repetition    (_, x), Repetition    (_, y)
  | NegLookahead  (_, x), NegLookahead  (_, y) -> compare x y
  | Repetition    _,      _                    -> ~-1
  | _,                    Repetition    _      -> 1
  | NegLookahead  _,      _                    -> ~-1
  | _,                    NegLookahead  _      -> 1
  | Concatenation (_, x), Concatenation (_, y)
  | Union         (_, x), Union         (_, y) -> ListUtil.compare compare x y
  | Concatenation _,      _                    -> ~-1
  | _,                    Concatenation _      -> 1


type freq = Always | Sometimes | Never

let freq_stringer out freq = match freq with
  | Always    -> out "Always"
  | Sometimes -> out "Sometimes"
  | Never     -> out "Never"

module Lookahead = struct
  type t = {
    matches    : freq;
    prefix     : Range.Set.t list;
    min_length : int;
    max_length : int option;
  }

  let empty = {
    matches    = Always;
    prefix     = [];
    min_length = 0;
    max_length = Some 0;
  }

  let never = { empty with matches = Never }

  let stringer out { matches; prefix; min_length; max_length; } =
    Stringer.orec4
      "matches"      freq_stringer Sometimes
      "prefix"
        (Stringer.list (fun out rs -> out (CodeUnit.ranges_to_string rs)))
        []
      "min_length"   Stringer.int 0
      "max_length"   (Stringer.option Stringer.int) None
      out (matches, prefix, min_length, max_length)

  let concat a b =
    let matches = match a.matches, b.matches with
      | Never,     _
      | _,         Never     -> Never
      | Always,    Always    -> Always
      | Sometimes, _
      | _,         Sometimes -> Sometimes in
    match matches with
      | Never -> never
      | _     ->
        let min_length = a.min_length + b.min_length in
        let max_length = match a.max_length, b.max_length with
          | Some i, Some j -> Some (i + j)
          | _              -> None in
        let prefix =
          if (Opt.equal (=) (Some a.min_length) a.max_length
              && a.min_length = List.length a.prefix) then
            a.prefix @ b.prefix
          else
            a.prefix in
        {
          matches;
          prefix;
          min_length;
          max_length;
        }

  let union a b = match a.matches, b.matches with
    | Always, _
    | _,      Never -> a
    | Never,  _     -> b
    | _     , _     ->
      let prefix =
        let rec union_prefix prefix_a prefix_b = match prefix_a, prefix_b with
          (* If both don't have lookahead to union then drop since we can't
             compute a super-charset. *)
          | [],         _
          | _,          []         -> []
          | hd_a::tl_a, hd_b::tl_b ->
            (Range.Set.union hd_a hd_b)::(union_prefix tl_a tl_b) in
        union_prefix a.prefix b.prefix in
      let max_length = match a.max_length, b.max_length with
        | Some i, Some j -> Some (max i j)
        | _              -> None in
      {
        matches    = b.matches;  (* a is Sometimes, b is (Sometimes | Always *)
        prefix;
        min_length = min a.min_length b.min_length;
        max_length;
      }
end

module LA = Lookahead

let rec lookahead re k = match re with
  | CharSet       (_, s) when Range.Set.is_empty s -> LA.never, 0
  | Union         (_, [])                          -> LA.never, 0
  | CharSet       (_, s) ->
    let len, prefix, matches, k' =
      if Range.Set.is_empty s then
        0, [],  Never,     0
      else if k = 0 then
        1, [],  Sometimes, 0
      else
        1, [s], Sometimes, k-1 in
    { LA.
      matches;
      prefix     = prefix;
      min_length = len;
      max_length = Some len;
    },
    k'
  | NegLookahead  (_, b) ->
    let la, _ = lookahead b 0 in
    let matches = (match la.LA.matches with
      | Always    -> Never
      | Sometimes -> Sometimes;
      | Never     -> Always) in
    { LA.empty with LA.matches = matches; },
    k
  | Concatenation (_, ls) ->
    let rec walk ls k = (match ls with
      | []                         -> LA.empty, k
      | Concatenation (_, ls )::tl -> walk (ls @ tl) k
      | Union         (_, [x])::tl -> walk (x::tl)   k
      (* It would be nice to subtract out negative lookaheads, but it's not
         obvious that we can do that for negative lookaheads with unions.
         We can handle the single charset case though. *)
      | NegLookahead  (_, CharSet (_, r))                  ::tl
      | NegLookahead  (_, NegLookahead (_, CharSet (_, r)))::tl ->
        let tl_la, k' = walk tl k in
        (match tl_la.LA.matches with
          | Never -> tl_la, k'
          | _     ->
            let prefix = match tl_la.LA.prefix with
              | []     -> []
              | hd::tl ->
                let op = match (List.hd ls) with
                  | NegLookahead (_, CharSet _) -> Range.Set.difference
                  | _                           -> Range.Set.intersection in
                (op hd r)::tl in
            { tl_la with LA.matches = Sometimes; prefix = prefix }, k'
        )
      | hd::tl ->
        let hd_la, k'  = lookahead hd k in
        let tl_la, k'' = walk      tl k' in
        let c_la = LA.concat hd_la tl_la in
        (
          c_la,
          match c_la.LA.matches with
            | Never -> 0
            | _     -> k''
        )
    ) in
    walk ls k
  | Union         (_, hd::tl) ->
    let rec walk (la, k') ls = match ls with
      | [] -> la, k'
      | option::tl -> (match la.LA.matches with
          | Always -> la, k'  (* Short circuit *)
          | _      ->
            let option_la, k'' = lookahead option k in
            let la' = LA.union la option_la in
            walk (la', min k' k'') tl
      ) in
    walk (lookahead hd k) tl
  | Repetition    (_, b) -> (match lookahead b k with
      | { LA.matches    = Never;  _ },       _  -> LA.never, 0
      | { LA.max_length = Some 0; _ } as la, k' -> la, k'
      | la,                                  k' ->
        { la with LA.max_length = None }, k'
    )

let lookahead k re = fst (lookahead k re)

type ('m, 'i) application = {
  regex   : 'm t;
  inp     : 'i list;
  (** chain of string slices to be matched against. *)
  out_rev : 'i list;
  (** regions matched in reverse order. *)
}

let application_stringer input_stringer out { regex; inp; out_rev } =
  Stringer.rec3
    "regex"   stringer
    "inp"     (Stringer.list input_stringer)
    "out_rev" (Stringer.list input_stringer)
    out (regex, inp, out_rev)

let _ = application_stringer  (* For debugging. *)

type 'i reader = {
  is_empty : 'i -> bool;
  read     : 'i -> CodeUnit.t * 'i * 'i;
  join     : 'i -> 'i -> 'i;
  stringer : 'i Stringer.t;
  start_of : 'i -> 'i;
  compare  : 'i -> 'i -> int;
  empty    : 'i;
}

let str_cursor_reader = {
  is_empty = StrCursor.is_empty;
  read     = (
    fun inp ->
      let cu, after = StrCursor.read inp in
      cu, StrCursor.trunc inp after, after
  );
  join     = (
    fun left right -> StrCursor.trunc left (StrCursor.limit right)
  );
  stringer = StrCursor.stringer;
  start_of = (fun inp -> StrCursor.trunc inp inp);
  compare  = StrCursor.compare;
  empty    = StrCursor.empty;
}

type 'i applier = 'i reader -> ?is_eof:bool -> 'i list -> 'i Match.t

(*
   A single step reduction semantics for regular expressions.

   Irreducable forms are
   1. No match
     [] - no match
   2. Complete match ends at cursor
     [{ regex: Concatenation (_, []); inp = _::_; _ }]
   3. Complete match consumed all input
     [{ regex: Concatenation (_, []); inp = [];   _ }]
   4. Match in progress that is paused for lack of input
     [{ regex: CharSet _;             inp = [];   _ }]

   Other forms might be continued with extra input slices.
*)
let small_step reader =
  let rec small_step stack = match stack with
    | [] -> invalid_arg "empty"
    | { regex = CharSet _; inp = []; _ }::tl -> fail_over tl
    | ({ regex = CharSet (_, rs); inp = inp_hd::inp_tl; out_rev } as hd)::tl ->
      if reader.is_empty inp_hd then
        let out_rev' = match inp_tl with
          | []      -> out_rev
          (* Make sure that the head of out_rev is always a cursor into
             the same backing buffer as the head of inp. *)
          | next::_ -> (reader.start_of next)::out_rev in
        ({ hd with inp = inp_tl; out_rev = out_rev' })::tl
      else begin
        let cu, before_and_at, after = reader.read inp_hd in
        if reader.compare inp_hd after >= 0 then
          failwith (Printf.sprintf "non-monotonic selector %s >= %s"
                      (Stringer.s reader.stringer inp_hd)
                      (Stringer.s reader.stringer after));
        if Range.Set.has rs cu then
          let out_rev' =
            (reader.join (List.hd out_rev) before_and_at)::(List.tl out_rev) in
          pass_over { hd with inp = after::inp_tl; out_rev = out_rev' } tl
        else
          fail_over tl
      end
    | ({ regex = Concatenation (_, []); _ } as hd)::tl -> pass_over hd tl
    | ({ regex = Concatenation (m, cat_hd::cat_tl); _ } as hd)::tl ->
      { hd with regex = cat_hd }
      ::{ hd with regex = Concatenation (m, cat_tl) }
      ::tl
    | { regex = Union (_, []); _ }::tl -> fail_over tl
    | ({ regex = Union (m, unn_hd::unn_tl); _ } as hd)::tl ->
      { hd with regex = unn_hd }
      ::{ hd with regex = Union (m, unn_tl) }
      ::tl
    | ({ regex = NegLookahead (_, body); _ } as hd)::_ ->
      (* Just push b and then invert the result in fail_over / pass_over *)
      { hd with regex = body }::stack
    | ({ regex = Repetition (_, body); _ } as hd)::_ ->
      (* Just push body and then on the first success, insert a fake union ("")
         operator, and on any success, repush the body. *)
      { hd with regex = body }::stack
  and fail_over stack = match stack with
    | { regex = Union _; _ }::_ ->
      small_step stack
    | ({ regex = NegLookahead (m, _); _ } as hd)::tl ->
      (* Replace the lookahead with a node that always passes.
         This way, a nested negative lookahead (a positive lookahead) will
         pass instead of infinitely looping. *)
      { hd with regex = Concatenation (m, []) }::tl
    | _::tl -> fail_over tl
    | [] -> []
  and pass_over hd tl = match hd with
    (* Proceed with next element in concatenation *)
    | { regex = Concatenation (_, _::_); _ } -> small_step (hd::tl)
    (* Grab defeat from the jaws of victory. *)
    | { regex = NegLookahead _; _ } -> fail_over tl
    (* Store the input and output state from the last iteration in a
       placeholder and ensure that failure of the second or subsequent
       iterations will lead to success of the repetition as a whole. *)
    | { regex = Repetition (_, body); inp; _ }->
      let rec same_pos inp_a inp_b = match inp_a, inp_b with
        | [], []                 -> true
        | a_hd::a_tl, b_hd::b_tl ->
          (* Don't tail recurse because a_hd and b_hd can only be compared if
             they are slices into the same input which is true when a_tl and
             b_tl have the same length. *)
          same_pos a_tl b_tl && reader.compare a_hd b_hd = 0
        | _                      -> false in
      let made_progress = match tl with
        | [] -> true
        | follower::_ -> not (same_pos inp follower.inp) in
      (* Since we succeeded once, greedily push any success into any containing
         option which will allow us to gracefully exit on failure after an
         initial success. *)
      let tl' = match tl with
        | ({ regex = Union (_, Concatenation (_, [])::_); _ } as t_hd)::t_tl ->
          { t_hd with inp = hd.inp; out_rev = hd.out_rev }::t_tl
        | _ ->
          let m = meta hd.regex in
          { hd with regex = Union (m, [Concatenation (m, [])]) }::tl in
      if made_progress then
        (* Re-enqueue if progress was made. *)
        { hd with regex = body }::hd::tl'
      else
        pass_over (List.hd tl') (List.tl tl')
    | _ -> (match tl with
        | [] -> [{ hd with regex = Concatenation (meta hd.regex, []) }]
        | t_hd::t_tl ->
          pass_over ({ t_hd with inp = hd.inp; out_rev = hd.out_rev }) t_tl) in
  small_step

let apply_at re reader ?(is_eof=false) inp =
  let small_step = small_step reader in
  let rec big_step ~is_eof stack = match stack with
    (* Fully reduced *)
    | [] -> Match.NoMatch
    | [{ regex = Concatenation (_, []); out_rev; inp; _ }] ->
      Match.Complete
        {
          Match.before = [];
          Match.at     = List.rev out_rev;
          Match.after  = inp;
        }
    (* Pause when we've exhausted the input. *)
    | { regex = CharSet _; inp = []; out_rev }::_ when not is_eof ->
      (* Pause but allow resumption. *)
      let out = List.rev out_rev in
      let continuation ?(is_eof=false) inps =
        let stack' = List.map
          (fun x -> match x.inp with
            | [] ->
              let out_rev' = List.rev_append
                (List.map (fun inp -> reader.start_of inp) inps)
                x.out_rev in
              { x with inp = inps;          out_rev=out_rev' }
            | _ ->
              { x with inp = (x.inp @ inps) })
          stack in
        big_step ~is_eof:is_eof stack' in
      Match.Prefix ({ Match.before = []; Match.at = out; Match.after = [] },
                    continuation)
    (* Otherwise, process one step. *)
    | _ -> big_step ~is_eof:is_eof (small_step stack) in
  let inp = match inp with
    | [] -> [reader.empty]
    | _  -> inp in
  let start_stack =
    let inp0 = List.hd inp in
    [{ regex = re; inp = inp; out_rev=[reader.start_of inp0] }] in
  big_step ~is_eof:is_eof start_stack

(* Kick off an attempt to match at the beginning of cursors. *)
let rec scan_forward re reader ~is_eof before_rev cursors =
  wrap_continuation re reader
    (fun ?(is_eof=false) nc ->
      assert (List.length nc = 0); apply_at re reader ~is_eof:is_eof cursors)
    ~is_eof:is_eof before_rev cursors []
(* wrap the continuation so that it tries to continue if no match is found at
   the current start point. *)
and wrap_continuation
    re reader cont before_rev cursors ?(is_eof=false) new_cursors =
  match cont ~is_eof:is_eof new_cursors with
    | Match.NoMatch ->
      keep_scanning re reader ~is_eof:is_eof before_rev (cursors@new_cursors)
    | Match.Complete bounds ->
      Match.Complete { bounds with Match.before = List.rev before_rev }
    | Match.Prefix (bounds, cont) ->
      let cont' = wrap_continuation
        re reader cont before_rev (cursors@new_cursors) in
      Match.Prefix  ({ bounds with Match.before = List.rev before_rev }, cont')
(* Stop matching at cursors and advance the read head so we can try another
   match. *)
and keep_scanning re reader ~is_eof before_rev cursors = match cursors with
  | [] -> Match.NoMatch
  | start_cursor::rest ->
    let before_rev', cursors' =
      if reader.is_empty start_cursor then
        (match rest with
          | []         -> before_rev
          | hd_rest::_ -> (reader.start_of hd_rest)::before_rev),
        rest
      else
        let _, before_and_at, after = reader.read start_cursor in
        (reader.join (List.hd before_rev) before_and_at)::(List.tl before_rev),
        after::rest in
    (* cursors' might be empty at this point but we still
       recurse so that negative lookaheads have a chance to match. *)
    scan_forward re reader ~is_eof:is_eof before_rev' cursors'

let apply_after re reader ?(is_eof=false) cursors =
  let before_rev = match cursors with
    | []    -> []
    | hd::_ -> [reader.start_of hd] in
  scan_forward re reader ~is_eof:is_eof before_rev cursors


(* Regular expression ambiguity under concatenation helps optimize out checks
   that avoid token merging conflicts. *)
(* TODO: Do proper translation of the regex to an NFA and check whether
   (suff(L) pref(R)) has a non-empty intersection with R.

   http://cs.stackexchange.com/questions/10852
   /for-regular-languages-a-and-b-determine-whether-b-might-match-early-in-a-b

   TODO: Can we be conservative while ignoring negative AND positive lookaheads?
*)
module Ambiguity = struct
  type t = Unambiguous | Ambiguous

  let compare a b = match a, b with
    | Unambiguous, Unambiguous -> 0
    | Unambiguous, _           -> ~-1
    | _,           Unambiguous -> 1
    | Ambiguous,   Ambiguous   -> 0

  let stringer out x = match x with
    | Unambiguous -> out "Unambiguous"
    | Ambiguous   -> out "Ambiguous"
end

let concat_ambiguity chars lt rt = begin
  (* Treat it as unambiguous when:
     1. lt has a fixed length, OR
     2. rt starts with a character that cannot appear in lt
     We cannot depend on
     i. lt ends with a character that cannot appear in rt because that
        would spuriously appear unambiguous under concatenation when
        lt matches a lt_s, rt matches rt_s which is a substring of lt_s
        but not a suffix of lt_s.
  *)
  let la_lt = lookahead lt 1 in
  let _ = chars in
  (match la_lt with
    | { LA.min_length; max_length=Some max_length_i; _ }
        when min_length = max_length_i ->
      Ambiguity.Unambiguous
    | _ ->
      let la_rt = lookahead rt 1 in
      (match la_rt with
        | { LA.prefix = first_rt_char::_; min_length; _ }
            when min_length <> 0 ->
          (* test whether first_rt_char can appear in lt *)
          let rec can_match_early r = begin match r with
            | CharSet       (_, s)  ->
              CodeUnit.Range.Set.intersects first_rt_char s
            | Repetition    (_, b)  -> can_match_early b
            | Concatenation (_, ls)
            | Union         (_, ls) -> List.exists can_match_early ls
            | NegLookahead  _       -> false
          end in
          if can_match_early lt then
            Ambiguity.Ambiguous
          else
            Ambiguity.Unambiguous
        | _ -> Ambiguity.Ambiguous
      )
  )
end


let to_unique_string cuk re = begin
  let buf = ByteOutput.Buffer.make () in
  let buf_end () = ByteOutput.Buffer.length buf in
  (* True when the string on buf is the unique string matched. *)
  let rec append_unique_string re = begin
    match re with
      | CharSet       (_, r)      ->
        if CodeUnit.Range.Set.is_singleton r then begin
          CodeUnitKind.emit cuk (Opt.require (CodeUnit.Range.Map.min r)) buf;
          true
        end else
          false
      | Concatenation (_, ls)     ->
        List.for_all append_unique_string ls
      | NegLookahead  _           -> true
      (* Below is of marginal value. *)
      | Repetition    (_, b)      ->
        (* True when b is always empty *)
        let pos = buf_end () in
        append_unique_string b && (pos = buf_end ())
      | Union         (_, [])     -> false
      | Union         (_, hd::tl) ->
        let before = buf_end () in
        if append_unique_string hd then begin
          let after = buf_end () in
          let first_unique = ByteOutput.Buffer.sub buf before after in
          (* True when each branch produces the same output. *)
          List.for_all
            (fun branch ->
              let same_output = (
                append_unique_string branch
                && str_eq
                  first_unique
                  (ByteOutput.Buffer.sub buf after (buf_end ()))
              ) in
              ByteOutput.Buffer.truncate buf after;
              same_output
            )
            tl
        end else
          false
  end in
  if append_unique_string re then
    Some (ByteOutput.Buffer.to_string buf)
  else
    None
end
