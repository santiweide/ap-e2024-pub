-- origin
Atom ::= var|int|bool|"("Exp")" 

FExp ::= Atom
        | FExp FExp

LExp ::= FExp
        | "if" Exp "then" Exp "else" Exp LExp
        | "\" var "->" Exp
        | "try" Exp "catch" Exp
        | "let" var "=" Exp "in" Exp

Exp ::=  LExp
        | Exp "==" Exp -- lowest
        | Exp "+" Exp
        | Exp "-" Exp
        | Exp "*" Exp Exp "/" Exp
        | Exp "**" Exp -- highest
        | "print" string Atom
        | "get" Atom
        | "put" Atom Atom

-- transformed: 





Atom ::= var
       | int
       | bool
       | "(" Exp ")"

FExp ::= Atom
        | Atom Atom

LExp ::= FExp
        | "if" Exp "then" Exp "else" Exp
        | "\" var "->" Exp
        | "try" Exp "catch" Exp
        | "let" var "=" Exp "in" Exp

Exp2 ::= (* empty *)
        | "**" Atom Exp

Exp1 ::= (* empty *)
        | "*" Atom Exp
        | "/" Atom Exp

Exp0 ::= (* empty *)
        | "+" Atom Exp
        | "-" Atom Exp

Exp00 ::= (* empty *)
        | "==" Atom Exp

-- 

Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp1' ::= (* empty *)
        | "*" Atom Exp1'
        | "/" Atom Exp1'

Exp1 ::= Atom Exp1'

Exp0' ::= (* empty *)
        | "+" Exp1 Exp0'
        | "-" Exp1 Exp0'

Exp0 ::= Exp1 Exp0'

Exp  ::= Exp0

--
Exp :: = Exp0
Exp0 = Exp3 Exp2 Exp1 Exp0'
Exp1 = Atom Exp1'