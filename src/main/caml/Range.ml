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

module type END_POINT = sig
  type t
  val least : t
  val zero : t
  val next : t -> t
  val stringer : Stringer.sink -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type RANGE = sig
  type elt
  type t = private { lt : elt; rt : elt }
  type range_t = t
  exception Invalid_range of elt * elt
  val compare : t -> t -> int
  val left : t -> elt
  val right : t -> elt
  val make : elt -> elt -> t
  val make_incl : elt -> elt -> t
  val singleton : elt -> t
  val span : t -> t -> t
  val to_string : t -> string
  val stringer : t Stringer.t
  module Map : sig
    type 'a t
    exception Ranges_not_disjoint of range_t * range_t
    exception Not_in_range of elt
    val make : (range_t * 'a) list -> 'a t
    val range_idx : 'a t -> elt -> int option
    val left : 'a t -> int -> elt
    val right : 'a t -> int -> elt
    val min : 'a t -> elt option
    val max_excl : 'a t -> elt option
    val value : 'a t -> int -> 'a
    val maybe_get : 'a t -> elt -> 'a option
    val get : 'a t -> elt -> (unit -> 'a) -> 'a
    val require : 'a t -> elt -> 'a
    val has : 'a t -> elt -> bool
    val fold_left : ('b -> elt -> elt -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val fold_left_intersecting :
      ('b -> elt -> elt -> 'a -> 'b) -> 'b -> 'a t -> range_t -> 'b
    val fold_right : (elt -> elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val fold_right_intersecting :
      (elt -> elt -> 'a -> 'b -> 'b) -> 'a t -> range_t -> 'b -> 'b
    val iter : (elt -> elt -> 'a -> unit) -> 'a t -> unit
    val iter_intersecting :
      (elt -> elt -> 'a -> unit) -> 'a t -> range_t -> unit
    val map : (elt -> elt -> 'a -> 'b) -> 'a t -> 'b list
    val map_intersecting :
      (elt -> elt -> 'a -> 'b) -> 'a t -> range_t -> 'b list
    val map_map : (elt -> elt -> 'a -> 'b) -> 'a t -> 'b t
    val size : 'a t -> int
    val is_empty : 'a t -> bool
    val to_string : (range_t -> string) -> ('a -> string) -> 'a t -> string
    val stringer : 'a Stringer.t -> 'a t Stringer.t
    val compact_stringer : range_t Stringer.t -> 'a Stringer.t ->
      'a t Stringer.t
    val merge : ('a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val reduce2 :
      (elt -> elt -> 'a -> 'b option -> 'c option -> 'a)
      -> 'a -> 'b t -> 'c t -> 'a
    val union : 'a t -> 'a t -> 'a t
    val difference : 'a t -> 'b t -> 'a t
    val intersection_r : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val intersection : 'a t -> 'b t -> 'a t
    val is_range_subset : 'a t -> 'b t -> bool
    val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
    val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  end
  module Set : sig
    type t = (unit Map.t)
    val make : range_t list -> t
    val of_map : 'a Map.t -> t
    val singleton : elt -> t
    val is_singleton : t -> bool
    val single_range : elt -> elt -> t
    val single_range_incl : elt -> elt -> t
    val has : 'a Map.t -> elt -> bool
    val iter : (elt -> elt -> unit) -> 'a Map.t -> unit
    val map : (elt -> elt -> 'a) -> 'b Map.t -> 'a list
    val fold_left : ('a -> elt -> elt -> 'a) -> 'a -> t -> 'a
    val size : t -> int
    val is_empty : t -> bool
    val union : 'a Map.t -> 'b Map.t -> t
    val difference : 'a Map.t -> 'b Map.t -> t
    val intersection : 'a Map.t -> 'b Map.t -> t
    val intersects : 'a Map.t -> 'b Map.t -> bool
    val contains_all : 'a Map.t -> 'b Map.t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val to_string :
      ?combine:(range_t option -> string list -> string) ->
      ?bound:('a Map.t -> range_t option) ->
      ?range_to_string:(range_t -> string) ->
      'a Map.t -> string
    val compact_stringer : range_t Stringer.t -> t Stringer.t
    val stringer : t Stringer.t
    val empty : t
  end
end

module Make (EP : END_POINT) : RANGE with type elt=EP.t = struct

  type elt = EP.t

  type t = { lt : elt; rt : elt }

  type range_t = t

  exception Invalid_range of elt * elt

  let compare a b =
    let delta = EP.compare a.lt b.lt in
    if delta <> 0 then delta else EP.compare a.rt b.rt

  (* Set up comparison to happen by end-point. *)
  let (>=@)  a b = EP.compare a b >= 0
  let (<@)   a b = EP.compare a b <  0
  let (<=@)  a b = EP.compare a b <= 0
  let (=@)   a b = EP.equal   a b
  let (<@>)  a b = not (a =@ b)
  let max_ep a b = if EP.compare a b >= 0 then a else b
  let min_ep a b = if EP.compare a b <= 0 then a else b
  let _ = min_ep

  let val_eq = Pervasives.(=)

  let left r = r.lt

  let right r = r.rt

  let make left right =
    if EP.compare left right < 0 then
      { lt = left; rt = right; }
    else
      raise (Invalid_range (left, right))

  let make_incl left incl_right = make left (EP.next incl_right)

  let singleton el = make el (EP.next el)

  let make_range = make

  let span a b = make a.lt b.rt

  let to_string r =
    Printf.sprintf "[%s, %s)"
      (Stringer.s EP.stringer r.lt) (Stringer.s EP.stringer r.rt)

  let stringer out r =
    if EP.equal (EP.next r.lt) r.rt then
      Stringer.call1 "Range.singleton" EP.stringer out r.lt
    else
      Stringer.call2 "Range.make" EP.stringer EP.stringer out (r.lt, r.rt)

  let range_to_string = to_string

  let range_stringer = stringer

  module Map = struct

    type 'a t = ((elt array) * ('a array))

    exception Ranges_not_disjoint of range_t * range_t

    exception Not_in_range of elt

    let make entries =
      match entries with
      | [] -> ([||], [||])
      | (_, value0) :: _ ->
          let n = List.length entries in
          let endpoints = Array.make (n * 2) EP.zero in
          let values = Array.make n value0 in
          let emit (s:elt) (e:elt) v i =
            if (i = 0
               || (endpoints.((i * 2) - 1) <@ s)
               || (not (val_eq v values.(i - 1)))) then
              (endpoints.(i * 2) <- s;
               endpoints.((i * 2) lor 1) <- e;
               values.(i) <- v;
               i + 1)
            else
              (endpoints.((i * 2) - 1) <-
                 (max_ep e endpoints.((i * 2) - 1)); i) in
          let rec merge i entries =
            (match entries with
             | [] ->
                 if i = n then
                   (endpoints, values)
                 else
                   ((Array.sub endpoints 0 (i*2)), (Array.sub values 0 i))
             | ({ lt = left; rt = right }, value) :: rest ->
                 merge (emit left right value i) rest) in
            merge 0
              (List.stable_sort
                (fun (r0, _) (r1, _) -> compare r0 r1)
                entries)

    let is_empty (_, values) = (0 = Array.length values)

    let size (_, values) = Array.length values

    (** Inclusive indices in values of all&only ranges in m that intersect r *)
    let ranges_intersecting (endpoints, _) { lt; rt } =
      let left = BinSearch.binsearch  ~arr:endpoints ~cmp:EP.compare lt in
      let right = BinSearch.binsearch ~arr:endpoints ~cmp:EP.compare rt in
      (
        (* Given m = { [-100, -99) => (), [1, 3) => () }      *)
        (* Given start of range x               0  1  2  3  4 *)
        (* binsearch x in [|-100; -99; 1; 3|]  -3  2 -4  3 -5 *)
        (* lnot binsearch                       2 -3  3 -4  4 *)
        (* which maps to                        1  1  1  2  2 *)
        (* as handled by case below             d  a  c  b  c *)
        (* a. even and non-negative : start point :
              left_incl = left/2 *)
        (* b. odd  and non-negative : end point   :
              left_incl = (left+1)/2 = (left / 2) + 1 *)
        (* c. even and negative : inv in range    :
              left_incl = (lnot left)/2 *)
        (* d. odd  and negative : inv btw ranges  :
              left_incl = (lnot left)/2 *)
        (((if left < 0 then lnot left else left + (left land 1))) / 2),
        (* even and non-negative : start point :
              right_excl = right/2 *)
        (* odd  and non-negative : end point :
              right_excl = (right/2)+1 = (right+1)/2 *)
        (* even and negative : inv in range :
              right_excl = ((lnot right)/2)+1 = ((lnot right)+1)/2 *)
        (* odd  and negative : inv btw ranges :
              right_excl = (lnot right)/2 *)
        (* Since (i + 1) / 2 = i / 2 for all even integer i *)
        let pos_right = if right < 0 then lnot right else right in
        (pos_right / 2) + ((pos_right land 1) - 1))

    let fold_left_between f initial (endpoints, values) first last =
      let rec fold v i =
        if i > last then v else
        (fold (f v endpoints.(i * 2) endpoints.((i * 2) lor 1) values.(i))
          (i + 1)) in
      fold initial first

    let fold_left f initial ((_, values) as m) =
      fold_left_between f initial m 0 ((Array.length values) - 1)

    let fold_left_intersecting f initial m r =
      let left_incl, right_incl = ranges_intersecting m r in
      fold_left_between f initial m left_incl right_incl

    let fold_right_between f (endpoints, values) initial first last =
      let rec fold i v =
        if i > last then v else
        (f endpoints.(i * 2) endpoints.((i * 2) lor 1) values.(i)
         (fold (i + 1) v)) in
      fold first initial

    let fold_right f ((_, values) as m) initial =
      fold_right_between f m initial 0 ((Array.length values) - 1)

    let fold_right_intersecting f m r initial =
      let left_incl, right_incl = ranges_intersecting m r in
      fold_right_between f m initial left_incl right_incl

    let map f m = fold_right (fun s e v tail -> (f s e v)::tail) m []

    let map_intersecting f m r =
      fold_right_intersecting (fun s e v tail -> (f s e v)::tail) m r []

    let map_map f (ranges, values) =
      (ranges, Array.mapi (fun i v -> f ranges.(i*2) ranges.(i*2+1) v) values)

    let iter f m = fold_left (fun () s e v -> f s e v) () m

    let iter_intersecting f m r =
      fold_left_intersecting (fun () s e v -> f s e v) () m r

    let range_idx (endpoints, _) i =
      let j = BinSearch.binsearch ~arr: endpoints ~cmp: EP.compare i in
      (* There are four cases to consider:
         1. non-negative and even : found a start point.  Include.
         2. non-negative and odd  : found an end point.  Exclude.
         3. negative     and even : insert before end point.  Include.
         4. positive     and odd  : insert between ranges.  Exclude.
       *)
      if 0 = j land 1 then Some ((if j < 0 then lnot j else j) / 2) else None

    let left (endpoints, _) idx = endpoints.(idx * 2)

    let right (endpoints, _) idx = endpoints.((idx * 2) lor 1)

    let min (endpoints, _) =
      if Array.length endpoints = 0 then
        None
      else
        Some endpoints.(0)

    let max_excl (endpoints, _) =
      let n = Array.length endpoints in
      if n = 0 then
        None
      else
        Some endpoints.(n - 1)

    let value (_, values) idx = values.(idx)

    let maybe_get (((_, values) as m)) i = match range_idx m i with
      | Some j -> Some values.(j)
      | _ -> None

    let has m i = match range_idx m i with | None   -> false | Some _ -> true

    let get (((_, values) as m)) i default = match range_idx m i with
      | Some j -> values.(j)
      | _ -> default()

    let require (((_, values) as m)) i = match range_idx m i with
      | Some j -> values.(j)
      | _ -> raise (Not_in_range i)

    let combine f initial (endpoints0, values0) (endpoints1, values1) =
      let n0 = Array.length values0 in
      let n1 = Array.length values1 in
      let rec comb i0 i1 x limit =
        if (i0 = n0) && (i1 = n1) then
          x
        else if (i0 < n0) && (limit >=@ endpoints0.((i0 * 2) lor 1)) then
          (* we have completely processed m0.(i0) *)
          comb (i0 + 1) i1 x limit
        else if (i1 < n1) && (limit >=@ endpoints1.((i1 * 2) lor 1)) then
          (* we have completely processed m1.(i1) *)
          comb i0 (i1 + 1) x limit
        else if (i0 < n0) &&
          ((i1 = n1) || (endpoints0.((i0*2) lor 1) <@ endpoints1.(i1 * 2))) then
          (* m0.(i0) has not been completely processed and doesn't overlap   *)
          (* anything in m1.(i1)                                             *)
          let start0 = endpoints0.(i0*2) in
          let end0 = endpoints0.((i0*2) + 1) in
          comb (i0 + 1) i1 (f (max_ep limit start0) end0 i0 ~-1 x) end0
        else if (i1 < n1) &&
          ((i0 = n0) || (endpoints1.((i1*2) lor 1) <@ endpoints0.(i0 * 2))) then
          (* m1.(i1) has not been completely processed and doesn't overlap   *)
          (* anything in m0.(i0)                                             *)
          let start1 = endpoints1.(i1*2) in
          let end1 = endpoints1.((i1*2) lor 1) in
          comb i0 (i1 + 1) (f (max_ep limit start1) end1 ~-1 i1 x) end1
        else (
          (* overlapping ranges. Use the value from m0 where overlapping but *)
          (* where there are gaps in m0 fill them with the values from m1    *)
          let start0 = max_ep limit endpoints0.(i0*2) in
          let end0 = endpoints0.((i0*2) lor 1) in
          let start1 = max_ep limit endpoints1.(i1*2) in
          let end1 = endpoints1.((i1*2) lor 1) in
          let nend = if end0 <=@ end1 then end0 else end1 in
          if start0 =@ start1 then
            (* There is an overlapping section past limit *)
            comb i0 i1 (f start0 nend i0 i1 x) nend
          else if start0 <@ start1 then
            (* A gap in a mutually overlapping section filled by m0 *)
            comb i0 i1 (f start0 start1 i0 ~-1 x) start1
          else
            (* A gap in a mutually overlapping section filled by m1 *)
            comb i0 i1 (f start1 start0 ~-1 i1 x) start0)
      in comb 0 0 initial EP.least

    let reduce2 f v0 ((_, values0) as m0) ((_, values1) as m1) =
      combine
        (fun lt rt i0 i1 v ->
          f lt rt v
            (if i0 < 0 then None else Some values0.(i0))
            (if i1 < 0 then None else Some values1.(i1)))
        v0 m0 m1

    let merge f (ranges0, values0) (ranges1, values1) =
      let n0 = Array.length values0 in
      let n1 = Array.length values1 in
      let rec merge_ranges ranges_rev n_ranges pos i0 i1 =
        let maxpos x = if x >=@ pos then x else pos in
        let apply_f i0' i1' lt rt v0 v1 = match f v0 v1 with
          | None   -> merge_ranges ranges_rev n_ranges rt i0' i1'
          | Some v ->
            let ranges_rev', n_ranges' = match ranges_rev with
              | (lt_p, rt_p, v_p)::tl when lt =@ rt_p && (val_eq v_p v) ->
                (lt_p, rt,   v_p)::tl, n_ranges
              | _ -> (lt, rt, v)::ranges_rev, n_ranges+1 in
            merge_ranges ranges_rev' n_ranges' rt i0' i1' in
        if i0 = n0 then begin
          if i1 = n1 then
            ranges_rev, n_ranges
          else
            let lt1, rt1 = maxpos ranges1.(i1 * 2), ranges1.((i1 * 2) lor 1) in
            if lt1 >=@ rt1 then
              merge_ranges ranges_rev n_ranges pos i0 (i1 + 1)
            else
              apply_f i0 (i1 + 1) lt1 rt1 None (Some values1.(i1))
        end else if i1 = n1 then begin
          let lt0, rt0 = (maxpos ranges0.(i0 * 2)), ranges0.((i0 * 2) lor 1) in
          if lt0 >=@ rt0 then
            merge_ranges ranges_rev n_ranges pos (i0+1) i1
          else
            apply_f (i0+1) i1 lt0 rt0 (Some values0.(i0)) None
        end else begin
          let lt0, rt0 = maxpos ranges0.(i0 * 2), ranges0.((i0 * 2) lor 1) in
          if lt0 >=@ rt0 then
            merge_ranges ranges_rev n_ranges pos (i0+1) i1
          else
            let lt1, rt1 = maxpos ranges1.(i1 * 2), ranges1.((i1 * 2) lor 1) in
            if lt1 >=@ rt1 then
              merge_ranges ranges_rev n_ranges pos i0 (i1+1)
            else begin
              let min a b = if EP.compare a b <= 0 then a else b in
              let delta = EP.compare lt0 lt1 in
              if delta < 0 then
                apply_f (if rt0 <=@ lt1 then i0+1 else i0) i1 lt0 (min rt0 lt1)
                  (Some values0.(i0)) None
              else if delta <> 0 then
                apply_f i0 (if rt1 <=@ lt0 then i1+1 else i1) lt1 (min rt1 lt0)
                  None (Some values1.(i1))
              else
                apply_f i0 i1 lt0 (min rt0 rt1)
                  (Some values0.(i0)) (Some values1.(i1))
            end
        end in
      let ranges_rev, n_ranges = merge_ranges [] 0 EP.least 0 0 in
      match ranges_rev with
        | [] -> ([||], [||])
        | (lt, _, v)::_ ->
          let merged_ranges = Array.make (n_ranges * 2) lt in
          let merged_values = Array.make n_ranges       v in
          let rec fill i ranges_rev = match ranges_rev with
            | [] -> ()
            | (lt, rt, v)::tl ->
              let i' = i-1 in
              merged_ranges.(i' * 2)         <- lt;
              merged_ranges.((i' * 2) lor 1) <- rt;
              merged_values.(i')             <- v;
              fill i' tl in
          fill n_ranges ranges_rev;
          (merged_ranges, merged_values)

    let union (((_, values0) as m0)) (((_, values1) as m1)) =
      if 0 = (Array.length values1) then m0
      else if 0 = (Array.length values0) then m1
      else
        let val_merger a b = match a with | Some _ -> a | _ -> b in
        merge val_merger m0 m1

    let difference (((_, values0) as m0)) (((_, values1) as m1)) =
      if (0 = (Array.length values1)) || (0 = (Array.length values0)) then m0
      else
        let val_merger a b = match b with | Some _ -> None | _ -> a in
        merge val_merger m0 m1

    let intersection_r val_mer (((_, values0) as m0)) (((_, values1) as m1)) =
      if (0 = (Array.length values0)) || (0 = (Array.length values1))
      then ([||], [||])
      else
        let val_merger a b =
           match a with
           | Some x ->
               (match b with | Some y -> Some (val_mer x y) | _ -> None)
           | _ -> None in
        merge val_merger m0 m1

    let intersection m0 m1 = intersection_r (fun a _ -> a) m0 m1

    let is_range_subset m0 m1 =
      combine (fun _ _ i0 _ x -> x && i0 >= 0) true m0 m1

    let compare compare_values (a_indices, a_values) (b_indices, b_values) =
      let na = Array.length a_indices in
      let nb = Array.length b_indices in
      let rec cmp i =
        if i = na then
          if i = nb then 0 else -1
        else if i = nb then
          1
        else
          let delta = EP.compare a_indices.(i) b_indices.(i) in
          if delta <> 0 then delta
          else
            let delta = EP.compare a_indices.(i+1) b_indices.(i+1) in
            if delta <> 0 then delta
            else
              let delta =
                compare_values a_values.(i lsr 1) b_values.(i lsr 1) in
              if delta <> 0 then delta
              else
                cmp (i+2) in
      cmp 0

    let equal eq_values (a_indices, a_values) (b_indices, b_values) =
      let na = Array.length a_indices in
      let nb = Array.length b_indices in
      na = nb
      && ArrayUtil.for_all2 EP.equal  a_indices b_indices
      && ArrayUtil.for_all2 eq_values a_values  b_values

    let to_string str_range str_value m =
      let f s e v = (str_range (make_range s e)) ^ (" => " ^ (str_value v)) in
      "{" ^ ((String.concat ", " (map f m)) ^ "}")

    let stringer value_stringer out x =
      Stringer.call1 "Range.Map.make"
        (Stringer.list (Stringer.tup2 range_stringer value_stringer))
        out (map (fun lt rt v -> (make_range lt rt, v)) x)

    let compact_stringer range_stringer value_stringer out x =
      out "{";
      ignore (fold_left
        (fun i lt rt v ->
          if i <> 0 then out ";";
          range_stringer out (make_range lt rt);
          out "=>";
          value_stringer out v;
          i+1)
        0
        x);
      out "}"

  end

  module Set = struct

    type t = unit Map.t

    let make ranges = Map.make (List.map (fun r -> (r, ())) ranges)

    let empty = [||], [||]

    let of_map (indices, _) =
      let n = Array.length indices in
      if n = 0 then
        empty
      else
        let rec merge_indices i last =
          if i = n then
            indices
          else if last <@> indices.(i) then
            merge_indices (i+2) indices.(i+1)
          else begin
            (* We have two adjacent ranges with distinct values that should be
               combined. *)
            (* First, count the number of range pairs to merge. *)
            let rec count_dupes total i last =
              if i = n then total else
              let total = if last =@ indices.(i) then (total+1) else total in
              count_dupes total (i+2) indices.(i+1) in
            let n' = n - (2 * (count_dupes 1 (i+2) indices.(i+1))) in
            (* Second, allocate an array of the appropriate size. *)
            let merged = Array.make n' indices.(0) in
            merged.(1) <- indices.(1);
            (* Third, populate it. *)
            let rec merge_into i j last =
              if i < n then
                let lt, rt = indices.(i), indices.(i+1) in
                if lt =@ last then begin
                  merged.(j-1) <- rt;
                  merge_into (i+2) j rt;
                end else begin
                  merged.(j) <- lt;
                  merged.(j+1) <- rt;
                  merge_into (i+2) (j+2) rt;
                end in
            merge_into 2 2 indices.(1);
            merged
          end in
        let merged = merge_indices 2 indices.(1) in
        (merged, Array.make ((Array.length merged) / 2) ())

    let single_range lt rt = Map.make [(make_range lt rt, ())]

    let single_range_incl lt rt = single_range lt (EP.next rt)

    let singleton i = single_range i (EP.next i)

    let is_singleton s =
      (Map.size s) = 1 && (Map.right s 0) =@ (EP.next (Map.left s 0))

    let has s i = Map.has s i

    let iter f s = Map.iter (fun s e _ -> f s e) s

    let map f s = Map.map (fun s e _ -> f s e) s

    let fold_left f v0 s = Map.fold_left (fun v s e () -> f v s e) v0 s

    let is_empty s = Map.is_empty s

    let size s = Map.size s

    let union m0 m1 =
      if Map.is_empty m1 then
        of_map m0
      else if Map.is_empty m0 then
        of_map m1
      else
        let val_merger a b = match a with
          | Some _ -> Some ()
          | None   -> Opt.map ignore b in
        Map.merge val_merger m0 m1

    let difference m0 m1 =
      if Map.is_empty m0 then
        empty
      else if Map.is_empty m1 then
        of_map m0
      else
        let val_merger a b = match b with
          | Some _ -> None
          | _ -> (match a with
              | Some _ -> Some ()
              | _ -> None) in
        Map.merge val_merger m0 m1

    let intersects m0 m1 =
      Map.combine (fun _ _ i0 i1 x -> x || (i0 >= 0 && i1 >= 0)) false m0 m1

    let intersection m0 m1 = Map.intersection_r (fun _ _ -> ()) m0 m1

    let contains_all m0 m1 = is_empty (difference m1 m0)

    let compare a b = Map.compare (fun _ _ -> 0) a b
    let equal a b = Map.equal (fun _ _ -> true) a b

    let to_string
        ?(combine = fun _ ranges -> "{" ^ (String.concat ", " ranges) ^ "}")
        ?(bound = fun _ -> (None : range_t option))
        ?(range_to_string = range_to_string)
        set =
      let f (s : elt) (e : elt) = range_to_string (make_range s e) in
      let pos : string = combine None (map f set) in
      match bound set with
        | None -> pos
        | Some bounds ->
          let negation = difference (make [bounds]) set in
          let neg = combine (Some bounds) (map f negation) in
          if String.length neg <= String.length pos then neg else pos

    let stringer out x =
      if is_empty x then
        out "empty"
      else if is_singleton x then
        Stringer.call1 "Range.Set.singleton" EP.stringer out (Map.left x 0)
      else
        Stringer.call1 "Range.Set.make"
          (Stringer.list range_stringer)
          out (map (fun lt rt -> make_range lt rt) x)

    let compact_stringer range_stringer out x =
      out "{";
      ignore (fold_left
        (fun i lt rt ->
          if i <> 0 then out ";";
          range_stringer out (make_range lt rt);
          i + 1)
        0 x);
      out "}"
  end

end
