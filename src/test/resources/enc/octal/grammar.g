start := @String ([<] chars [>]);
chars := @If{Deadline != panic | Goal != enc} (
           (@Char(@CharValue [0-9A-Za-z] | octal))*
         )
       // Fix Token Merging in EncToIL
       | @If{Deadline = panic & Goal=enc} (
             (@Char @CharValue [0-8A-Za-z])
           | (@Char ([\\] @ScalarValue ([1-3] [0-7] [0-7])))
           | (@Char ([\\] @ScalarValue ([0-7] [0-7]?))
              (@Char ([\\] @ScalarValue ([6] [0-7])))*)
         )*;
octal := ([\\] @ScalarValue ([0-3] ([0-7] [0-7]?)? | [4-7] [0-7]?));
