start := @Scope{A} (
           "w"
           (
             ( @Set{A, x} "x" | @Set{A, y} "y")
             ( @If{A = x} "x" )?
             "."
           )
           | "z"
         );
