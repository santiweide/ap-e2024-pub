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

-- transformed into tail-recursion

string ::= just string
var ::= some special string

Exp ::= FExp LExp Exp0'

FExp ::= Atom FExp'
FExp' ::= (* empty *)
        | Atom FExp'

LExp ::= (* empty *)
       | "if" Exp "then" Exp "else" Exp
       | "\" var "->" Exp
       | "try" Exp "catch" Exp
       | "let" var "=" Exp "in" Exp

Exp0' ::= Exp3 Exp2 Exp1 Exp0

Exp3 ::= Atom Exp3'
Exp3' ::= (* empty *)
        | "**" Atom Exp3'

Exp2 ::= Atom Exp2'
Exp2' ::= (* empty *)
        | "*" Atom Exp2'
        | "/" Atom Exp2'

Exp1 ::= Atom Exp1'
Exp1' ::= (* empty *)
        | "+" Atom Exp1'
        | "-" Atom Exp1'

Exp0 ::= Atom Exp0'
Exp0' ::= (* empty *)
        | "==" Atom Exp0'

Atom ::= var
       | int
       | bool
       | "(" Exp ")"
       | "get" Atom 
       | "put" Atom Atom
       | "print" string Atom
