{
  R  <: (r,  rp);
  RW <: (rw, rwp);
  W  <: (w,  wp);
}

start :=
    @Scope{R}
    @Scope{W}
    @Scope{RW}
    @Set{R, r}
    @Entrust{extern_fn, R, RW} @Set{W, w} @Set{RW, rw} "foo";
