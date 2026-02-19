# Gramatica concreta
CA -> 'Automaton' IDENT '{' 'States' ':=' STATES 'Neighborhood' ':=' NEIGHBORHOOD  'Transition' '{' RULE '}' 'Default' ':=' IDENT '}'

STATES -> STATE | STATE '|' STATES
STATE -> IDENT ':' COLOR
COLOR -> 'white' | 'black' | ...

NEIGHBORHOOD -> LITCOORD | LITCOORD '|' NEIGHBORHOOD
LITCOORD -> '(' INT ',' INT ')'

RULE -> STATEEXP | 'if' BOOLEXP 'then' RULE 'else' RULE | 'case' '{' COND '}' | 'let' IDENT '=' 
COND -> 'otherwise' ':' '{' RULE '}' | BOOLEXP ':' '{' RULE '}' | BOOLEXP ':' '{' RULE '}' COND

STATEEXP -> IDENT | 'cell' | 'nei' '(' NAT ')'

INTEXP -> INT | 'neighbors' '(' STATEEXP ')' | '-' INTEXP | IDENT

BOOLEXP -> 'False' | 'True' | BOOLEXP 'and' BOOLEXP | BOOLEXP 'or' BOOLEXP | 'not' BOOLEXP
            | INTEXP '==' INTEXP | INTEXP '<=' INTEXP | INTEXP '<' INTEXP | INTEXP '>=' INTEXP
            | INTEXP '>' INTEXP | INTEXP '!=' INTEXP | STATEEXP '==' STATEEXP
            | STATEEXP '!=' STATEEXP | STATEEXP 'in' '[' SET ']'

SET -> STATEEXP | STATEEXP ',' SET

IDENT -> letter IDENT | letter
NAT -> D | NATD
D -> '0' | ... | '9'
INT -> NAT | '-'NAT


# desambiguacion de intexp (orden de precedencia -u > *,/ > +,-b ):
INTEXP -> INTEXP '+' INTTERM | INTEXP '-' INTTERM | INTTERM

INTTERM -> INTTERM '*' INTFACTOR | INTTERM '/' INTFACTOR | INTFACTOR

INTFACTOR -> '-' INTFACTOR | INTATOM

INTATOM -> INT | 'neighbors' '(' STATEEXP ')' | '(' INTEXP ')'

# eliminacion de recursion a izquierda:
INTEXP -> INTTERM INTEXP'
INTEXP' -> '-' INTTERM INTEXP' | '+' INTTERM INTEXP' | empty

INTTERM -> INTFACTOR INTTERM'
INTTERM' -> '*' INTFACTOR INTTERM' | '/' INTFACTOR INTTERM' | empty

INTFACTOR -> '-' INTFACTOR | INTATOM

INTATOM -> INT | 'neighbors' '(' STATEEXP ')' | '(' INTEXP ')'



# desambiguacion de boolexp (orden de precedencia ==,!=,<,<=,>,>=,in > not > and > or):
BOOLEXP -> BOOLEXP 'or' BOOLTERM

BOOLTERM -> BOOLTERM 'and' BOOLFACTOR

BOOLFACTOR -> 'not' BOOLFACTOR | BOOLATOM

BOOLATOM -> '(' BOOLEXP ')' | INTEXP '==' INTEXP | INTEXP '<=' INTEXP | INTEXP '<' INTEXP
           | INTEXP '>=' INTEXP | INTEXP '>' INTEXP | INTEXP '!=' INTEXP | STATEEXP '==' STATEEXP
           | STATEEXP '!=' STATEEXP | STATEEXP 'in' '[' SET ']'

SET -> STATEEXP | STATEEXP ',' SET

# eliminacion de recursion a izquierda:
BOOLEXP -> BOOLTERM BOOLEXP'
BOOLEXP' -> 'or' BOOLTERM BOOLEXP' | empty

BOOLTERM -> BOOLFACTOR BOOLTERM'
BOOLTERM' -> 'and' BOOLFACTOR BOOLTERM' | empty

BOOLFACTOR -> 'not' BOOLFACTOR | BOOLATOM

BOOLATOM -> (' BOOLEXP ')' | INTEXP '==' INTEXP | INTEXP '<=' INTEXP | INTEXP '<' INTEXP
           | INTEXP '>=' INTEXP | INTEXP '>' INTEXP | INTEXP '!=' INTEXP | STATEEXP '==' STATEEXP
           | STATEEXP '!=' STATEEXP | STATEEXP 'in' '[' SET ']'

SET -> STATEEXP | STATEEXP ',' SET



# Gramatica abstracta

AUTOMATA -> IDENT SPECS
SPECS -> STATES NEIGHBORHOOD RULE

STATES -> STATE | STATE STATES
STATE -> IDENT COLOR

NEIGHBORHOOD -> LITCOORD | LITCOORD NEIGHBORHOOD
LITCOORD -> INT INT

RULE -> STATEEXP | if BOOLEXP then RULE else RULE | case COND
COND -> otherwise : RULE | BOOLEXP : RULE COND  <---- todo case debe ser exhaustivo

STATEEXP -> IDENT | cell | nei(NAT)

INTEXP -> INT | neighbors(STATEEXP) | INTEXP + INTEXP | INTEXP - INTEXP | - INTEXP
            | INTEXP * INTEXP | INTEXP / INTEXP
