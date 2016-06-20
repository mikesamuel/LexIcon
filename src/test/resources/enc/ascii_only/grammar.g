start := @String ((@Char ch)*);
ch := @CharValue (ascii - ([\\] | @If{Deadline = panic} [ ]))
    | "\\" @ScalarValue (hex hex hex hex)
      ([ ] | @If{Goal != enc | Deadline != panic} ())
    | @CharValue{"\\"} "\\\\"
