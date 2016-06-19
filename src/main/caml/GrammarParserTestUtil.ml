include DisableGenericCompare

let default_ns = Identifier.Namespace.default
let blank_headers = {
  Grammar.
  namespace         = default_ns;
  grammar_variables = Var.Decls.empty
}

let tree_stringer out t =
  let headers_opt = match t with
    | Grammar.G (Grammar.Grammar (_, hdrs, _)) -> Some hdrs
    | _                                        -> None in
  let rec stringer out t =
    let name, data_stringer = Grammar.(match t with
      | G (Grammar (_,  ({ namespace; grammar_variables } as hdrs), _)) ->
        "G",
        (fun out ->
          let defaults = blank_headers in
          if true || not (Grammar.Equal.headers hdrs defaults) then
            Stringer.orec2
              "namespace" Identifier.Namespace.stringer defaults.namespace
              "grammar_variables" Var.Decls.stringer defaults.grammar_variables
              out (namespace, grammar_variables))
      | P (Production (_, id, _)) -> "P", (fun out -> Identifier.stringer out id)
      | A (Data _) -> "A.Data", ignore
      | A (Denormalized (_, p)) ->
        "A.Denormalized", (fun out -> Var.Pred.stringer out p)
      | A (Embedded (_, p)) ->
        "A.Embedded", (fun out -> Var.Pred.stringer out p)
      | A (CaseFold f) -> "A.CaseFold", (fun out -> CaseFold.stringer out f)
      | A (Override _) -> "A.Override", ignore
      | A (Scope (n, r)) ->
        "A.Scope",
        (fun out ->
          Stringer.tup2 Var.Name.stringer Grammar.Recursivity.stringer
            out (n, r))
      | A (Set (n, e)) ->
        let domain_opt = match headers_opt with
          | Some { grammar_variables=decls; _ } -> Var.Decls.domain decls n
          | None                                -> None in
        "A.Set",
        (fun out ->
          Var.Name.stringer out n;
          out ",";
          out (Printf.sprintf "/*%s*/"
                 (Stringer.s (Var.Expr.make_stringer domain_opt) e));
          Var.Expr.repr_stringer out e);
      | A (If p) ->
        "A.If", (fun out -> Var.Pred.stringer out p)
      | A (Until _) ->
        "A.Until", ignore
      | A (Entrust (nm, rs, p)) ->
        "A.Entrust",
        (fun out ->
          Stringer.tup3 Identifier.stringer Var.Names.stringer Var.Pred.stringer
            out (nm, rs, p))
      | N (Annotation _) -> "N.Annotation", ignore
      | N (CharSet _ as s) ->
        "N.CharSet", (fun out -> GrammarParser.body_stringer out s)
      | N (Difference _) -> "N.Difference", ignore
      | N (Concatenation _) -> "N.Concatenation", ignore
      | N (Reference (_, id)) ->
        "N.Reference", (fun out -> Identifier.stringer out id)
      | N (Repetition _) -> "N.Repetition", ignore
      | N (Union (_, o, _)) ->
        "N.Union", (fun out -> Grammar.Ordering.stringer out o)
      | N (Panic _) -> "N.Panic", ignore
    ) in
    out "(";
    out name; data_stringer out;
    Grammar.fold (fun _ n -> stringer out n) () t;
    out ")" in
  stringer out t

let tree_printer prefix g =
  Printf.sprintf "\n\t%s %s\n" prefix (Stringer.s tree_stringer (Grammar.G g))
