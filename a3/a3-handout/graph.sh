Atom ::= var|int|bool|"("Exp")" 

FExp ::= Atom
        | FExp FExp

LExp ::= FExp
        | "if" Exp "then" Exp "else" Exp LExp

Exp ::=  LExp
        | Exp “==” Exp -- lowest
        | Exp "+" Exp
        | Exp "-" Exp
        | Exp "*" Exp Exp "/" Exp
        | Exp “**” Exp -- highest
