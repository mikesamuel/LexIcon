start := @Scope{X} @Scope{Y} (look_and_set_x set_y look_and_check_y);
look_and_set_x :=
    @Set{X, a} [a]
  | @Set{X, b} [b];
set_y :=
    @If{X = a} @Set{Y, c} ()
  | @If{X = b} @Set{Y, d} ();
look_and_check_y :=
    @If{Y = c} [c]
  | @If{Y = d} [d];
