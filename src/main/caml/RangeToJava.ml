include DisableGenericCompare

module JExpr = JavaParseTree.JExpr
let small_int_val = JExpr.small_int_val
let type_int = JavaParseTree.(JPrimType JInt)

type range_check = {
  unsigned:  bool;
  (** If true then we may assume that input cannot be negative. *)
  java_type: JavaParseTree.JType.t;
  (** The java type of the input that is checked against the range. *)
  ranges:    IL.OpenRange.Set.t;
  (** The ranges to check the input against. *)
}

let range_check_stringer out { unsigned; java_type; ranges } = Stringer.orec3
  "unsigned"  Stringer.bool                false
  "java_type" JavaParseTree.JType.stringer type_int
  "ranges"    IL.OpenRange.Set.stringer    IL.OpenRange.Set.empty
  out (unsigned, java_type, ranges)


module RangeSetMap = MapUtil.Make (IL.OpenRange.Set)

let range_checks_canon { unsigned; java_type; ranges } = begin
  let sminval, maxval = JavaParseTree.(match java_type with
    | JPrimType JChar  -> IL.Point            0, IL.Point     0xFFFF
    | JPrimType JByte  -> IL.Point ~-      0x80, IL.Point       0x7F
    | JPrimType JShort -> IL.Point ~-    0x8000, IL.Point     0x7FFF
    | JPrimType JInt   -> IL.Point ~-0x80000000, IL.Point 0x7FFFFFFF
    (* jlong max is not representable as an IL.Point. *)
    | _                -> IL.LeftInfinity,       IL.RightInfinity ()) in
  let minval = if unsigned then IL.Point 0 else sminval in
  (* Do some trickery to clamp a range without running into problems when
     Point Pervasives.max_int = maxval. *)
  let broader_ranges r = begin
    let r' = IL.OpenRange.Set.make (
      IL.OpenRange.Set.fold_left
        (fun ranges lt rt ->
          if (IL.OpenEndPoint.compare rt minval <= 0
              || IL.OpenEndPoint.compare lt maxval >= 0) then
            ranges
          else
            let broadened_range = IL.OpenRange.make
              (
                if IL.OpenEndPoint.compare minval lt < 0 then lt
                else                                          IL.LeftInfinity
              )
              (
                if IL.OpenEndPoint.compare maxval rt > 0 then rt
                else                                          IL.RightInfinity ()
              ) in
            broadened_range::ranges)
        [] r
    ) in
    let uses_unsignedness =
      unsigned && not (IL.OpenEndPoint.equal sminval (IL.Point 0))
      && IL.OpenRange.Set.has r' (IL.LeftInfinity)
      && not (IL.OpenRange.Set.has r (IL.LeftInfinity)) in
    {
      unsigned = uses_unsignedness;
      java_type;
      ranges = r'
    }
  end in
  let positive_range_check = broader_ranges ranges in
  let negative_range_check = broader_ranges (IL.invert_open_range_set ranges) in
  let key =
    if IL.OpenRange.Set.has positive_range_check.ranges IL.LeftInfinity then
      positive_range_check.ranges
    else
      negative_range_check.ranges in
  (positive_range_check, negative_range_check, key)
end
(** [range_checks_canon range_check] is
    [(canonical_check, canonical_inverted_check, key)] *)


let simplify_ranges_via_masks = begin
  let is_pos_power_of_2 i = i > 0 && (i land (i - 1)) = 0 in
  let integral_log2 =
    let log2 = Array.make 1024 0 in
    let rec populate k =
      let two_to_k = 1 lsl k in
      if two_to_k < Array.length log2 then begin
        log2.(two_to_k) <- k;
        populate (k+1)
      end
    in
    populate 0;
    let rec log2i p x =
      if x < Array.length log2 then
        Some (p + log2.(x))
      else
        log2i (p + 1) (x lsr 1)
    in
    fun x -> if is_pos_power_of_2 x then log2i 0 x else None
  in

  let popcount =
    (* TODO: batteries has an efficient popcount *)
    let rec pc n i =
      if i = 0 then
        n
      else
        pc (n + 1) (i land (i - 1))
    in
    pc 0
  in

  let enumerate_range_members size r = begin
    let members_arr = Array.make size 0 in
    let rec enumerate k i j =
      if i < j then begin
        members_arr.(k) <- i;
        enumerate (k + 1) (i + 1) j
      end else
        k
    in
    let k = IL.OpenRange.Set.fold_left
      (fun k lt rt -> match lt, rt with
        | IL.Point i, IL.Point j ->
          enumerate k i j
        | IL.LeftInfinity, _ | IL.RightInfinity _, _
        | _, IL.LeftInfinity | _, IL.RightInfinity _ ->
          failwith "size mismatch"
      )
      0 r
    in
    assert (k = size);
    members_arr
  end in

  let denumerate_range_members members = begin
    let rec ranges ranges_rev members = match members with
      | [] ->
        IL.OpenRange.Set.make
          (List.rev_map (fun (lt, rt) -> IL.OpenRange.make lt rt) ranges_rev)
      | hd::tl -> (match ranges_rev with
          | (lt, IL.Point rt)::rest when rt = hd ->
            ranges ((lt, (IL.Point (hd + 1)))::rest) tl
          | _ ->
            ranges ((IL.Point hd, IL.Point (hd + 1))::ranges_rev) tl)
    in
    ranges [] members
  end in

  let cardinality r = IL.OpenRange.Set.fold_left
    (fun n_opt lt rt -> match n_opt, lt, rt with
      | Some n, IL.Point i, IL.Point j -> Some (n + j - i)
      | _ -> None)
    (Some 0) r
  in

  let max_size = 128 in
  let max_k = Opt.require (integral_log2 max_size) in
  assert (max_k = 7);

  (*
    For example, [x = 'a' || x = 'A'] can be reduced to [(x & ~32) == 'A'] since
    ['a'] and ['A'] differ by 0x32 and the number of bits in 0x32 is 1, and the
    number of distinct values is 2**1.

    Algorithm:
    let max_size = 128
    let max_k be log2 max_size
    let members = distinct values in ranges
    let size = |members|
    if size <= 1 || size > max_size: return inputs unaltered
    for each k from max_k .. 1:
      # We want to collapse multiple groups down to one.  If our choice of k
      # does not evenly divide groups, then there is no mask that defines a
      # canonical group.
      if size % 2**k != 0: continue
      # If we only have one group, then we're not making progress.
      if size == 2**k: continue
      let n_groups = size / 2**max_k
      # There may be many ways to do this partition well, but we just naively
      # split them left to right.
      let groups = a partition of members whose elements have size group_size
      # Since we split left to right and members is ordered, group[0] is the
      # least member of a group.
      let mask = bitwise_or [groups[0][0] xor group[0] for group in groups]
      # See if our groups cover all possible mask values.
      if 2**(popcount(mask)) != n_groups: continue
      # Apply the mask to members and see if the groups are now equivalent.
      let masked_groups = [[m & ~mask for m in group] for group in groups]
      if not (all masked_groups equal element-wise): continue
      return mask, masked_groups[0]
  *)
  fun input_expr ({ ranges; unsigned; java_type=_ } as range_check) -> begin
    (* If we invert the range check, we might be able to match more. *)
    let invert, ranges, size_opt =
      let clamp ranges =
        (* Treat negative values as don't cares. *)
        if unsigned then
          IL.OpenRange.Set.intersection ranges
            (IL.OpenRange.Set.single_range (IL.Point 0) (IL.RightInfinity ()))
        else
          ranges
      in
      let pranges = clamp ranges in
      match cardinality pranges with
        | Some n -> false, pranges, Some n
        | None   ->
          let nranges = clamp (IL.invert_open_range_set ranges) in
          match cardinality nranges with
            | Some n -> true,  nranges, Some n
            | None   -> false, ranges,  None
    in
    match size_opt with
      | Some size when size <= max_size && size > 1 ->
        let members = enumerate_range_members size ranges in
        (* Try for different values of k to split the members into groups
           and find a mask that allows collapsing into one group by taking
           advantage of don't care bits. *)
        let rec k_split k = begin
          if k < 0 then
            None
          else
            let n_in_group = 1 lsl k in
            if size mod n_in_group <> 0 || size = n_in_group then
              k_split (k - 1)
            else begin
              let n_groups = size / n_in_group in
              let groups =
                let rec split i =
                  if i = n_groups then
                    []
                  else
                    (Array.sub members (n_in_group * i) n_in_group)
                    ::(split (i + 1))
                in
                split 0
              in
              (* Compare each groups first member and come up with a mask of
                 bits that differ. *)
              let member0 = members.(0) in
              let mask = List.fold_left
                (fun mask group -> mask lor (member0 lxor group.(0)))
                0 groups
              in
              (* See if all possible values of the mask are represented among
                 the bits that differ. *)
              let mask_bit_count = popcount mask in
              if n_groups <> (1 lsl mask_bit_count) then
                None
              else begin
                (* Make sure that, after applying the mask, the groups are the
                   same. *)
                let masked_groups = List.map
                  (fun group ->
                    let masked_group = Array.copy group in
                    Array.iteri
                      (fun group_index value ->
                        masked_group.(group_index) <- value land (lnot mask))
                      masked_group;
                    masked_group)
                  groups
                in
                if false then
                  Printf.printf
                    ("\nk=%d\nmembers=%s\ngroups=%s\n"
                     ^^ "invert=%b\nmask=0x%x\nmask_bit_count=%d\n"
                     ^^ "n_groups=%d\nmasked_groups=%s\n"
                    )
                    k
                    (Stringer.s (Stringer.array Stringer.int) members)
                    (Stringer.s (Stringer.list (Stringer.array Stringer.int))
                       groups)
                    invert
                    mask
                    mask_bit_count
                    n_groups
                    (Stringer.s (Stringer.list (Stringer.array Stringer.int))
                       masked_groups);
                match masked_groups with
                  | masked_group0::tl
                    when List.for_all (ArrayUtil.equal (=) masked_group0) tl ->
                    if false then
                      Printf.printf "\tMASKED\n";
                    Some (Array.to_list masked_group0, mask)
                  | _ -> None
              end
            end
        end in
        (match k_split max_k with
          | None -> input_expr, range_check
          | Some (canon_group, mask) ->
            assert (mask = (mask land 0xFFFFFFFF));
            let input_expr' = JavaParseTree.(
              let imask = JConstant (JIntVal (Int32.of_int mask)) in
              JBinary (input_expr, JBitAndOp, JPrefix (JBitNegateOp, imask))
            ) in
            let ranges' = denumerate_range_members canon_group in
            let ranges' =
              if invert then
                IL.invert_open_range_set ranges'
              else
                ranges'
            in
            (
              input_expr',
              { range_check with ranges = ranges'; java_type=type_int }
            )
        )
      | _ -> input_expr, range_check
  end
end
(**
   [simplify_ranges_via_masks input_expr range_check] is
   [(input_expr, range_check)] or an [(input_expr & mask, simpler_range_check)]
   such that we can do fewer range check by ignoring bits that are not
   significant in the result.
 *)


let translate_range_check
    ~likely_ranges ~label_for_ranges ~declare_global ~declare_method =
begin
  let _ = declare_global in
  let usage_count = begin
    match likely_ranges with
      | None    -> RangeSetMap.empty
      | Some likely_range_list ->
        let count_uses rm rc =
          let p, n, k = range_checks_canon rc in
          let wider x y = Opt.unless type_int (JavaParseTree.JType.wider x y) in
          RangeSetMap.multiadd
            (0, JavaParseTree.(JPrimType JChar))
            (fun (cnt0, typ0) (cnt1, typ1) -> (cnt0 + cnt1, wider typ0 typ1))
            k (1, wider p.java_type n.java_type) rm in
        List.fold_left count_uses RangeSetMap.empty likely_range_list
  end in

  let translate_range_check declare_local il_typ input_expr range_check =
  begin
    let input_expr, range_check =
      simplify_ranges_via_masks input_expr range_check
    in
    (* Below we try various strategies for testing intersection.  Many need
       to use input_expr many times, so we allocate a local temporary where
       possible. *)
    let reuse_inp = begin
      let local = ref (match input_expr with
        | JavaParseTree.JLocalRef _ -> Some input_expr
        | _                         -> None) in
      fun () -> match !local, declare_local with
        (* Subsequent times through, reuse that local. *)
        | Some x, _             -> x
        (* The first time through, allocate a local variable if the
           input expression is not already a simple local reference,
           and assign to it in-band so we don't evaluate out-of-order. *)
        | None, Some decl_local ->
          let name = JavaParseTree.JIdent.make "el" in
          let name = decl_local ([], [], range_check.java_type, name) in
          let local_ref = JavaParseTree.JLocalRef name in
          local := Some local_ref;
          (* And assign input_expr to it. *)
          JavaParseTree.JBinary (
            local_ref, JavaParseTree.JAssignOp, input_expr
          )
        | None, None -> input_expr  (* Worst case, just re-evaluate. *)
    end in

    (* Translate between end-points and java expressions. *)
    let ep_val = match il_typ with
      | IL.CodeUnit_t CodeUnitKind.Octet
      | IL.CodeUnit_t CodeUnitKind.Utf16
      | IL.CodeUnit_t CodeUnitKind.Unicode ->
        (fun i ->
          if 0 <= i && i < 0x10000 then
            JavaParseTree.(JConstant (JCharVal i))
          else
              (* TODO: give a hint to the compiler to use hex. *)
            small_int_val i)
      | _ -> small_int_val in
    (* Applies a variety of strategies to check whether an integer is
       in a set of ranges. *)
    let rec range_to_java invert r =
      (match IL.OpenRange.(Map.min  r, Map.max_excl r, Set.size r) with
        (* Empty set. *)
        | None, _, 0 ->
        assert (IL.OpenRange.Set.is_empty r);
          JavaParseTree.JConstant (JavaParseTree.JBoolVal invert)
        | Some IL.LeftInfinity, Some (IL.RightInfinity ()), _ ->
          (* Invert the test since we can reduce the number of ranges by
             1 that way. When the range includes all points this reaches
             the previous case. *)
          range_to_java (not invert) (IL.invert_open_range_set r)
        | Some (IL.Point i),    Some (IL.Point j), 1
          when j-i = 1 ->
          JavaParseTree.JBinary (
            input_expr,
            (if invert then JavaParseTree.JNotEqualsOp
             else           JavaParseTree.JEqualsOp),
            ep_val i
          )
        | Some (IL.Point i),    Some (IL.Point j), c
          when j-i <= 64 && c > 1 ->
          (* We have 64 or fewer distinct points, so use a Java long or
             int as a bitmask. *)
          (* TODO: Maybe handle longer dense but fragmented bitmasks with
             boolean[]s if it is the case that the JIT can prove that
             private static final boolean[]s don't escape in the
             presence of Field.setAccessible(true). *)

          (* We use Int64 to store a mask since Ocaml ints are 63 bits
             on a good day. *)
          let (|:), (&:), (<<:) = Int64.(logor, logand, shift_left) in

          (* Convert the range to a bit mask. *)
          (* Integer corresponding to right-most bit. *)
          let offset = if i >= 0 && j < 64 then 0 else i in
          (* Compute a mask and count the number of set bits. *)
          let mask, n_set = IL.OpenRange.Set.fold_left
            (fun (mask, n_set) lt rt -> match lt, rt with
              | IL.Point lt, IL.Point rt ->
                let a, b = lt - offset, rt - offset in
                (mask |: ((Int64.pred (Int64.one <<: (b - a))) <<: a)),
                n_set + (b - a)
              | _ -> failwith "infinities between points")
            (Int64.zero, 0) r in

          let test = if invert then JExpr.boolean_inverse else fun x -> x in
          if n_set <= 3 then begin
            (* Just enumerate the values since we branch thrice below
               otherwise. *)
            let values = IL.OpenRange.Set.fold_left
              (fun values_rev lt rt -> match lt, rt with
                | IL.Point lt, IL.Point rt ->
                  let rec enum values_rev i j =
                    if j = i then values_rev
                    else          enum (i::values_rev) (i+1) j in
                  enum values_rev lt rt
                | _ -> failwith "infinities between points")
              [] r in
            let op =
              if invert then JavaParseTree.JNotEqualsOp
              else           JavaParseTree.JEqualsOp in
            let tests = List.map
              (fun i ->
                JavaParseTree.JBinary(reuse_inp (), op, small_int_val i))
              (List.rev values) in
            (if invert then JExpr.nary_and else JExpr.nary_or) tests
          end else begin
            let lower_32 = Int64.pred (Int64.one <<: 32) in
            let fits_int32 = 0 = Int64.compare (mask &: lower_32) mask in
            let small_int_val_of_size i =
              if fits_int32 then
                small_int_val i
              else
                JavaParseTree.(JConstant (JLongVal (Int64.of_int i))) in
            (* Emit a bounds-check then a shift and mask. *)
            let e_shifted () =
              let rhs =
                if offset = 0 then
                  reuse_inp ()
                else
                  JavaParseTree.(
                    JBinary (reuse_inp (), JSubOp, small_int_val offset)) in
              JavaParseTree.(
                JBinary (small_int_val_of_size 1, JLShiftOp, rhs)) in
            (* According to
               docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html
               #jvms-6.5.ishl
               the shift amount is the lower 5 bits for the ishl
               (integer shift left) instruction, and for lshl
               (long shift left) the lower 6 bits, so we still need to
               bounds check.

               docs.oracle.com/javase/specs/jls/se7/html/jls-15.html
               #jls-15.19
               specifies the semantics of << in similar terms so it is
               safe to mask first before bounds checking which is
               worthwhile when the mask is sparse.
            *)
            JavaParseTree.(
              let shift_and_mask = JBinary (
                e_shifted (), JBitAndOp, JConstant (JLongVal mask)
              ) in
              let mask_test, left_limit_test, right_limit_test =
                JBinary (                    (* ((1 << e) & mask) != 0 *)
                  shift_and_mask,
                  JNotEqualsOp,
                  small_int_val_of_size 0),
                JBinary (                    (* && i <= e *)
                  ep_val i,
                  JLessEqOp,
                  reuse_inp ()),
                JBinary (                    (* && e <= (j-1) *)
                  reuse_inp (),
                  JLessEqOp,
                  ep_val (j-1))
              in
              let tests =
                if offset = 0 && i = 0 && range_check.unsigned then
                  (* Skip the left limit test if it can't fail. *)
                  [mask_test;                  right_limit_test]
                else
                  [mask_test; left_limit_test; right_limit_test]
              in
              test (JavaParseTree.JExpr.nary_and tests)
            )
          end
        | _ ->
          (* Turn into a || of single range checks. *)
          let point_count r = IL.OpenRange.Set.fold_left
            (fun n lt rt ->
              n
              + (match lt with | IL.Point _ -> 1 | _ -> 0)
              + (match rt with | IL.Point _ -> 1 | _ -> 0)
            )
            0 r in
          let unsigned = range_check.unsigned in
          let get_xe =
            (* Get rid of javac's unread local warnings since they may
               distract from other problems. *)
            if point_count r > 1 then reuse_inp else fun _ -> input_expr in
          let cmps_rev = IL.OpenRange.Set.fold_left
            (fun cmps_rev lt rt ->
              let lt =
                if unsigned then
                  Cmp.max IL.OpenEndPoint.compare (IL.Point 0) lt
                else
                  lt in
              let comparison = IL.(JavaParseTree.(match lt, rt with
                | LeftInfinity, RightInfinity _               ->
                  JavaParseTree.JExpr.value_true
                | Point 0,      RightInfinity _ when unsigned ->
                  JavaParseTree.JExpr.value_true
                | LeftInfinity, Point j                       ->
                  JBinary (get_xe (), JLessEqOp, ep_val (j-1))
                | Point i,      RightInfinity _               ->
                  JBinary (ep_val i,  JLessEqOp, get_xe ())
                | Point i,      Point j         when j-i = 1  ->
                  JBinary (get_xe (), JEqualsOp, ep_val i)
                | Point i,      Point j ->
                  let left_limit  = ep_val i in
                  let right_limit = ep_val (j - 1) in
                  let left_expr   = get_xe () in
                  let right_expr  = get_xe () in
                  JBinary (
                    JBinary (left_limit, JLessEqOp, left_expr),
                    JLogicalAndOp,
                    JBinary (right_expr, JLessEqOp, right_limit)
                  )
                | RightInfinity _, _ | _, LeftInfinity        ->
                  failwith "invalid range"
              )) in
              comparison::cmps_rev)
            [] r in
          assert (not (is_empty cmps_rev));
          let range_or = JExpr.nary_or (List.rev cmps_rev) in
          if invert then
            JExpr.boolean_inverse range_or
          else
            range_or
      ) in
    range_to_java false range_check.ranges
  end in

  let range_key_to_method
      : (JavaParseTree.jmethod_ref * range_check) RangeSetMap.t ref
      = ref RangeSetMap.empty in

  let matcher_method_and_ranges_opt il_typ range_check pos key =
    match declare_method with
      | None         -> None
      | Some declare ->
        (match RangeSetMap.find_opt key !range_key_to_method with
          | Some (_, c) as found
            when not (JavaParseTree.JType.is_wider pos.java_type c.java_type) ->
            found
          | _ ->
            (* Should we declare a method just for this check? *)
            let (usage_count, widest_java_type) =
              RangeSetMap.find_def key (max_int, type_int) usage_count in
            (* Trivial. *)
            if IL.OpenRange.Set.size range_check.ranges < 2 then
              None
            (* Only used once. *)
            else if usage_count = 1 then
              None
            else begin
              let method_range_check = {
                pos with
                java_type = (Opt.unless type_int
                               (JavaParseTree.JType.wider widest_java_type
                                  pos.java_type));
              } in
              let name = label_for_ranges method_range_check in
              let input_name = JavaParseTree.JIdent.make "x" in
              let body_expr = translate_range_check
                None il_typ
                (JavaParseTree.JLocalRef input_name) method_range_check in
              let ranges_stringer =
                let is_code_unit = match il_typ with
                  | IL.CodeUnit_t CodeUnitKind.Octet
                  | IL.CodeUnit_t CodeUnitKind.Utf16
                  | IL.CodeUnit_t CodeUnitKind.Unicode -> true
                  | _                                  -> false in
                IL.naked_open_range_set_stringer ~is_code_unit in
              let matcher_method : JavaParseTree.jmethod = JavaParseTree.(
                [JDocAtCode (
                  Stringer.s ranges_stringer method_range_check.ranges
                )],
                [],
                [`Public; `Static], [], JRetType (JPrimType JBoolean),
                JavaParseTree.JIdent.make (
                  Label.to_string ~style:Label.LowerCamelCase name
                ),
                (* formals: int x *)
                ([([], [], method_range_check.java_type, input_name)],
                 JInvariadic),
                JThrows [],
                Some (JReturn (Some body_expr))
              ) in
              let method_ref = declare matcher_method in
              let matcher_method = (method_ref, method_range_check) in
              range_key_to_method := RangeSetMap.add key matcher_method
                !range_key_to_method;
              Some matcher_method
            end
        ) in

  let to_expr ~declare_local il_typ input_expr range_check = begin
    let pos, _, key = range_checks_canon range_check in
    let matcher_method_and_range_opt = matcher_method_and_ranges_opt
      il_typ range_check pos key in
    match matcher_method_and_range_opt with
      | None                    ->
        translate_range_check declare_local il_typ input_expr range_check
      | Some (method_ref, m_pos) ->
        let call = JavaParseTree.JCall (method_ref, [input_expr]) in
        if IL.OpenRange.Set.equal pos.ranges m_pos.ranges then
          call
        else
          JExpr.boolean_inverse call
  end in

  to_expr
end
