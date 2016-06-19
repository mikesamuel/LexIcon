(** Utilities for {!IL} test harnesses. *)


module ProgramBuilder : sig
  type t = {
    fn_ : label -> formal list -> stmt -> unit;
    global_ : label -> typ -> unit;

    let_   : label -> typ -> expr -> stmt;
    block_ : stmt list -> stmt;
    alt_   : stmt list -> stmt;
    try_   : stmt -> stmt -> stmt;
    call_  : label -> expr list -> stmt;
    cond_  : pred -> stmt;
    loop_  : stmt -> pred -> stmt;
    panic_ : stmt;
    pass_  : stmt;
    fail_  : stmt;
    set_global_ : label -> expr -> stmt;
    set_ptr_ : label -> expr -> stmt;
    set_cursor_ : label -> expr -> stmt;
    append_ : label -> expr -> stmt;
    append_str_ : label -> string -> stmt;
    append_mks_ : label -> EvMarker.t list -> stmt;
    copy_to_ : label -> expr -> expr -> stmt;
    truncate_ : label -> expr -> stmt;
    incr_ : label -> expr -> stmt;
    incr_int_ : label -> int -> stmt;

    not_ : pred -> pred;
    and_ : pred list -> pred;
    or_ : pred list -> pred;
    true_ : pred;
    false_ : pred;
    is_ : expr -> typ -> pred;
    in_ : expr -> range -> pred;
    lt_ : expr -> expr -> pred;
    empty_ : expr -> pred;
    is_match_ : expr -> pred;
    to_bool_ : expr -> pred;

    ref_ : label -> expr;
    global_ref_ : label -> expr;
    str_lit_ : string -> expr;
    el_at_ : expr -> expr;
    key_at_ : expr -> expr;
    val_at_ : expr -> expr;
    itoa_ : expr -> expr;
    ftoa_ : expr -> expr;
    cptoa_ : expr -> expr;
    ntoa_ : expr -> ScalarCharValue.t -> expr;
    alloc_buffer_ : expr -> expr -> expr;
    freeze_buffer_ : expr -> expr;
    freeze_buffer_k_ : expr -> CodeUnitKind.t -> expr;
    slice_buffer_ : expr -> expr -> expr -> expr;
    slice_buffer_k_ : expr -> expr -> expr -> CodeUnitKind.t -> expr;
    true_e_ : expr;
    false_e_ : expr;
    int_ : int -> expr;
    enum_ : label -> label -> expr;
    enums_ : label -> label list -> expr;
    deref_ : expr -> expr;
    alloc_ptr_ : typ -> expr;
    start_of_ : expr -> expr;
    end_of_ : expr -> expr;
    read_ : expr -> expr;
    lookahead_ : expr -> expr -> expr;
    lookahead_i_ : expr -> int -> expr;
    lookahead_h_ : expr -> expr -> range -> expr;
    lookahead_ih_ : expr -> int -> range -> expr;
    find_at_ : regex -> expr -> expr -> expr;
    find_first_ : regex -> expr -> expr -> expr;
    start_of_match_ : expr -> expr;
    end_of_match_ : expr -> expr;
    make_match_1_ : expr -> expr;
    make_match_2_ : expr -> expr -> expr;
    snapshot_ : expr -> expr;
    copy_cursor_1_ : expr -> expr;
    copy_cursor_2_ : expr -> expr -> expr;
    to_prim_ : expr -> typ -> expr;
    atoi_ : expr -> NumberSystem.t -> expr;
    atoi_k_ : expr -> CodeUnitKind.t -> NumberSystem.t -> expr;
    succ_ : expr -> expr;
    nin_ : label -> expr list -> expr;

    t_inp_buffer_ : typ;
    t_inp_cursor_ : typ;
    t_inp_snapshot_ : typ;
    t_out_buffer_ : typ;
    t_match_ : typ;

    re_dot_ : regex;
    re_cat_ : regex list -> regex;
    re_or_ : regex list -> regex;
    re_char_ : char -> regex;
    re_chars_ : range -> regex;
    re_inv_chars_ : range -> regex;
    re_pos_la_ : regex -> regex;
    re_neg_la_ : regex -> regex;

    btw_ : int -> int -> range;
    btw_ch_ : char -> char -> range;
    char_ : char -> range;
    ranges_ : range list -> range;
    inv_range_ : range -> range;

    to_program : unit -> unit IL.program;
  }
  and formal = label * typ
  and typ = IL.ltype
  and label = string
  and stmt = scopes -> unit IL.stmt
  and pred = scopes -> IL.predicate
  and expr = scopes -> IL.actual
  and regex = CodeUnitKind.t -> unit Regex.t
  and range = CodeUnitKind.t -> IL.OpenRange.Set.t
  and scopes = {
    def_local       : label -> typ -> Scope.L.Idx.t;
    idx_of_local    : label -> Scope.L.Idx.t;
    idx_of_global   : label -> Scope.G.Idx.t;
    idx_of_function : label -> Scope.F.Idx.t;
  }

  val make : 'a Var.Decls.t -> CodeUnitKind.t -> t
end
