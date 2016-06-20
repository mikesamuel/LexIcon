start := @Scope{A} (set read);

set := @Set {A, x} [x]
     | @Set {A, y} [y]
     | @Set {A, z} [z]
     | @Set {A, d} ();

read := @If{A=x} [x]
      | @If{A=y} [y]
      | @If{A=z} [z]
      | @If{A=d} [^xyz];
