include DisableGenericCompare

let decorate_statement_reaches program = begin
  let IL.Program (globals, fns, main_fn_idx) = program in
  let knowledge_of =
    ILKnowledge.knowledge_when_stmt_reached ~globals ~fns ~main_fn_idx in
  Stringer.s
    (IL.SourceStringers.decorated
       (fun stmt_stringer fn_idx_opt stmt_address out stmt ->
         let knowledge_for addr = match fn_idx_opt with
           | None -> None
           | Some fn_idx ->
             let k, _, _ = knowledge_of fn_idx addr in
             Some k in
         let knowledge_opt = knowledge_for stmt_address in
         let child_knowledge_opt = knowledge_for (0::stmt_address) in
         (match knowledge_opt with
           | None   -> ()
           | Some k ->
             let base_k = Opt.unless
               ILKnowledge.Possibility.ignorance child_knowledge_opt in
             if 0 <> (ILKnowledge.Possibility.compare k base_k) then begin
               ILKnowledge.Possibility.stringer out k;
               out Stringer.no_break;
               out ":";
               out "\n"
             end
         );
         stmt_stringer out stmt;
       ))
    program
end
