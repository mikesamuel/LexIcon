include DisableGenericCompare

let sprintf = Printf.sprintf

let bmp_no_surrogates_cus = CodeUnit.Range.Set.make [
  CodeUnit.Range.make CodeUnit.zero            (CodeUnit.of_int 0xD800);
  CodeUnit.Range.make (CodeUnit.of_int 0xE000) (CodeUnit.of_int 0x10000);
]
(** The range of code-points in the Basic Multilingual Plane that are not
    also UTF-16 surrogates, which is the set of code-points that have the
    same encoding in UTF-16 as in UTF-32. *)

let maybe_invert_charset parse_kind cus =
  match CodeUnit.Range.Map.max_excl cus with
    | None        -> cus, false
    | Some max_cu ->
      if (CodeUnitKind.n_units parse_kind) = (CodeUnit.as_int max_cu) then
        let inverted_cus = CodeUnit.Range.Set.difference
          (CodeUnit.Range.Set.single_range CodeUnit.zero max_cu)
          cus
        in
        let all_in_bmp = CodeUnit.Range.Map.is_range_subset
          bmp_no_surrogates_cus inverted_cus
        in
        if all_in_bmp then
          inverted_cus, true
        else
          cus, false
      else
        cus, false

let rec can_use_java_util_regex parse_kind regex = match regex with
  | Regex.CharSet       (_, cus) ->
    let cus, _ = maybe_invert_charset parse_kind cus in
    (* java.util.regex.Pattern deals fine with everything in the BMP, but
       until JDK 7 has horribly broken supplemental code-point support,
       especially when parse_kind = Utf16, and even after, matching
       orphaned high surrogates is problematic. *)
    CodeUnit.Range.Map.is_range_subset
      bmp_no_surrogates_cus cus
  | Regex.Repetition    (_, b)
  | Regex.NegLookahead  (_, b)  -> can_use_java_util_regex parse_kind b
  | Regex.Concatenation (_, ls)
  | Regex.Union         (_, ls) ->
    List.for_all (can_use_java_util_regex parse_kind) ls

let rec branch_load regex = match regex with
  | Regex.CharSet    _          -> 0
  | Regex.NegLookahead  (_, b)
  | Regex.Repetition    (_, b)  -> branch_load b
  | Regex.Concatenation (_, ls) ->
    List.fold_left (fun n b -> n +     branch_load b) 0   ls
  | Regex.Union         (_, ls) ->
    List.fold_left (fun n b -> n + 1 + branch_load b) ~-1 ls

let should_use_java_util_regex parse_kind regex =
  can_use_java_util_regex parse_kind regex
  (* TODO: improve these heuristics by randomizing it and finding which
     regular expressions perform better when compiled under benchmarks. *)
  && branch_load regex > 2

let regex_to_java_pattern_string parse_kind regex =
  let buf = ByteOutput.Buffer.make () in
  let emit = ByteOutput.Buffer.append buf in
  let rec xlate regex = Regex.(match regex with
    | CharSet (_, chars) ->
      let chars, inverted = maybe_invert_charset parse_kind chars in
      let esc_cu cu =
        let cui = CodeUnit.as_int cu in
        if cui < 0x80 then begin
          let ch = char_of_int cui in
          (match ch with
            | '[' | ']' | '-' | '\\' | '^' ->
              emit "\\";
            | _ -> ());
          emit (String.make 1 ch)
        end else
          if cui < 0x10000 then
            emit (sprintf "\\u%04x" cui)
          else
            let comb_surr = cui - 0x10000 in
            let surr0 = 0xd800 lor (comb_surr lsr 10) in
            let surr1 = 0xdc00 lor (comb_surr land 0x3ff) in
            emit (sprintf "\\u%04x\\u%04x" surr0 surr1) in
      let esc_range lt rt = match CodeUnit.as_int rt - CodeUnit.as_int lt with
        | 0 -> esc_cu lt
        | 1 -> esc_cu lt; esc_cu rt
        | _ -> esc_cu lt; emit "-"; esc_cu rt in
      (match CodeUnit.Range.Map.max_excl chars with
        | None ->
          (* Don't trust [] to be interpreted as always-fails, but negative
             lookahead of the empty string always fails. *)
          if inverted then
            emit ("(?!)")
          else
            emit "."  (* Required DOTALL *)
        | Some _ ->
          (* Supplemental code-point support in java.util.regex not standard
             until JDK 7 so we fake it. *)
          let basic_plane_limit =
            CodeUnit.of_int CodeUnitKind.(n_units Utf16) in
          let wrote_supplemental  = ref false in
          (* Write basic plane code-points after supplemental since Pattern will
             happily match ([\u0000-\uffff]|[\ud800-\udbff][\udc00-\udfff])*
             against a supplemental code-point as two UTF-16 code-units. *)
          CodeUnit.Range.Set.iter
            (fun lt rt ->
              if CodeUnit.compare rt basic_plane_limit > 0 then begin
                if inverted then failwith "inverted";
                (* There are several cases to consider:
                   1. Single leading surrogate (\uD8XX[\uDCXX-\uDCxx])
                   2. Trailing surrogates cover whole range:
                   ([\uD8xx-\uD8xx][\uDC00-\uDFFF])
                   3. Ranges split:
                   ([\uD8xx][\uDCxx-\uDFFFF]
                   |[\uD8xx-\uDxxx][\uDC00-\uDFFF]
                   |[\uD8xx][\uDC00-\uDCxx])
                *)
                let dc00 = CodeUnit.of_int 0xdc00 in
                let dfff = CodeUnit.of_int 0xdfff in
                let sum = CodeUnit.sum in
                let rec write_surrogates lt_lead lt_trail rt_lead rt_trail =
                  if CodeUnit.equal lt_lead lt_trail then begin
                    esc_cu lt_lead;
                    if CodeUnit.equal lt_trail rt_trail then
                      esc_cu lt_trail
                    else begin
                      emit "["; esc_range lt_trail rt_trail; emit "]"
                    end
                  end else if CodeUnit.equal lt_trail dc00 then begin
                    if CodeUnit.equal rt_trail dfff then begin
                      emit "[";
                      esc_range lt_lead rt_lead;
                      emit "][\\udc00-\\udfff]"
                    end else begin
                      write_surrogates lt_lead dc00 (sum rt_lead ~-1) dfff;
                      emit "|";
                      write_surrogates rt_lead dc00 rt_lead           rt_trail
                    end
                  end else begin
                    write_surrogates lt_lead         lt_trail lt_lead dfff;
                    emit "|";
                    write_surrogates (sum lt_lead 1) dc00     rt_lead rt_trail
                  end in
                if !wrote_supplemental then
                  emit "|"
                else begin
                  wrote_supplemental := true;
                  emit "(?:"
                end;
                let surr_pair cu =
                  let cui = CodeUnit.as_int cu in
                  let cui_sub = cui - 0x10000 in
                  (CodeUnit.of_int (0xD800 lor (cui_sub lsr 10)),
                   CodeUnit.of_int (0xDC00 lor (cui_sub land 0x3FF))) in
                let bmp_lt = CodeUnit.Cmp.max basic_plane_limit lt in
                let lt_lead, lt_trail = surr_pair bmp_lt in
                let rt_lead, rt_trail = surr_pair (CodeUnit.sum rt ~-1) in
                write_surrogates lt_lead lt_trail rt_lead rt_trail
              end;
            )
            chars;
          let is_regex_meta cu =
            let cui = CodeUnit.as_int cu in
            cui >= 0x20 && cui < 0x80 &&
              match char_of_int cui with
                | '\\' | '[' | ']' | '(' | ')' | '{' | '}'
                | '.'  | '^' | '$' | '?' | '*' | '+' | '|'
                | '/' -> true
                | _   -> false in
          if (CodeUnit.Range.Set.is_singleton chars
              && not (is_regex_meta (CodeUnit.Range.Map.left chars 0)))
            && not inverted then
            let cu = CodeUnit.Range.Map.left chars 0 in
            esc_range cu cu
          else begin
            let wrote_basic_plane = ref false in
            CodeUnit.Range.Set.iter
              (fun lt rt ->
                if CodeUnit.compare lt basic_plane_limit < 0 then begin
                  if not !wrote_basic_plane then begin
                    if !wrote_supplemental then emit "|";
                    wrote_basic_plane := true;
                    emit (if inverted then "[^" else "[");
                  end;
                  let bmp_rt = CodeUnit.Cmp.min rt basic_plane_limit in
                  esc_range lt (CodeUnit.sum bmp_rt ~-1)
                end;
              )
              chars;
            if !wrote_basic_plane then emit "]";
            if !wrote_supplemental then emit ")"
          end
      )
    | Repetition (_, b) -> emit "(?:"; xlate b; emit ")++"
    | Concatenation (_, ls) ->
      List.iter
        (fun x -> match x with
          | Union _ -> emit "(?:"; xlate x; emit ")"
          | _       -> xlate x
        )
        ls
    | Union (_, []) -> emit "(?!)"
    | Union (_, ls) ->
      ignore (
        List.fold_left
          (fun need_pipe x ->
            if need_pipe then emit "|";
            (match x with
              | Union _ -> emit "(?:"; xlate x; emit ")"
              | _       -> xlate x);
            true
          )
          false ls
      )
    | NegLookahead (_, NegLookahead (_, b)) ->
      emit "(?=";
      xlate b;
      emit ")"
    | NegLookahead (_, b) ->
      emit "(?!";
      xlate b;
      emit ")"
  )
  in
  if should_use_java_util_regex parse_kind regex then begin
    xlate regex;
    Some (ByteOutput.Buffer.to_string buf)
  end else
    None
