fn match (opos : IData (InputCursor_t Unicode), limit : IData (InputSnapshot_t Unicode), match : SPtr (Match_t (Unanchored, Unicode))) {
  var pos : IData (InputCursor_t Unicode);
  var success : SPtr IBool_t;
  var start : IData (InputSnapshot_t Unicode);
  var found : SPtr IBool_t;
  let pos = copy_cursor (opos);
  let found = (new IBool_t);
  ptr found <- false;
  repeat {
    require pos < limit;
    let start = snapshot (pos);
    alt {
      {
        require (read (pos)) in (['<']) && (lookahead (pos, 7)) < limit && (read (lookahead (pos, 1))) in (['/']) && (read (lookahead (pos, 2))) in (['S'] ['s']) && (read (lookahead (pos, 3))) in (['C'] ['c']) && (read (lookahead (pos, 4))) in (['R'] ['r']) && (read (lookahead (pos, 5))) in (['I'] ['i']) && (read (lookahead (pos, 6))) in (['P'] ['p']) && (read (lookahead (pos, 7))) in (['T'] ['t']);
        let success = (new IBool_t);
        incr (pos, 8);
        alt {
          {
            require pos < limit && (read (pos)) in (['-'] ['0'-':'] ['A'-'Z'] ['a'-'z']);
            ptr success <- true
          } else {
            ptr success <- false
          }
        };
        require ! ((* (success)));
        ptr found <- true;
        ptr match <- make_match (start, pos)
      } else {
        incr pos
      }
    }
  }
  while ! ((* (found)));
  require (* (found))
}
fn sanitize (inp : IData (InputCursor_t Unicode), limit : IData (InputSnapshot_t Unicode), out : EData OutputBuffer_t) {
  var match : SPtr (Match_t (Unanchored, Unicode));
  let match = (new Match_t (Unanchored, Unicode));
  call match (inp, limit, match);
  require is_match (* (match));
  copy_to (inp, start_of_match (* (match)), out);
  set_cursor (inp, start_of_match (* (match)));
  append ("`", out);
  copy_to (inp, end_of_match (* (match)), out);
  append ("`", out)
}
