start := @Scope{Case} (x y z);

x := @ValueTrue @Set{Case,upper} "X" | @ValueFalse @Set{Case,lower} "x";
y :=            @If {Case=upper} "Y" |             @If {Case=lower} "y";
z :=            @If {Case=upper} "Z" |             @If {Case=lower} "z";
