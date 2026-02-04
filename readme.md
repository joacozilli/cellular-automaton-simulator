# Automata Celular
Sea A subconjunto de R^n. A es una red si es un subgrupo discreto de R^n; es decir, verifica que:
    - A es cerrado bajo la suma
    - existe ep > 0 tal que para todo x,y en A ||x - y|| >= ep (x /= y)


Un automata celular C se define formalmente como una 4-upla (A,S,N,T) donde:
    - A es una red
    - S es un conjunto finito de estados
    - N = {v1,...,vm} conjunto finito de vectores tal que x+vi esta en A para todo x en A
    llamado vecindad. Decimos que m es el tamanio de la vecindad en C.
    - T : S^m -> S funcion de transicion llamada regla local de C


La regla local la podemos generalizar a T : A -> S^m -> S; de esta forma podemos definir
distintas reglas locales para distintos vectores. NO SE HACE


Dado C = (A,S,N,T) automata celular, una configuracion de C es una funcion c : A -> S
que mapea a cada elemento de A un estado. Denominando X al conjunto de todas las configuraciones
posibles de C, C pasa de una configuracion a otra mediante una funcion de transicion global G
definida como:
G : X -> X
G c = \x -> let {v1,...,vm} = N
            in T (c (x+v1)) ... (c (x+vm))

con la generalizacion de funcion de transicion:
G : X -> X
G c = \x -> let {v1,...,vm} = N
            in T x (c (x+v1)) ... (c (x+vm))


En nuestro caso, trabajamos con redes finitas y bidimensionales. Resulta entonces que algunas
celdas de la red tendran vecindad incompleta. Posibles soluciones:
    - los vecinos faltantes se asumen en cierto estado predeterminado en toda configuracion
    - la red es periodica (circular vertical y horizontalmente)
La red siempre sera de la forma A = {(x,y) | x in [0,n-1], y in [0,m-1]}, de tamanio nxm a
eleccion por el usuario.



# Ejemplo: juego de la vida
- S = {0,1}

- N(x,y) = {(0,0),(1,0),(1,1),(0,1),(-1,0),(-1,-1),(0,-1),(1,-1),(-1,1)}

- T(0,s2,...,s9) = if alive(s2,...,s9) == 3 then 1 else 0
  T(1,s2,...,s9) = if alive(s2,...,s9) in {2,3} then 1 else 0

# Sintaxis del lenguaje
El usuario tiene que proveer un archivo .ca donde se especifique el conjunto de estados, la
funcion de vecindad, la funcion de transicion y la configuracion inicial.


Automaton GameOfLife {
    States := alive | dead

    Neighborhood := (0,0)|(1,0)|(1,1)|(0,1)|(-1,0)|(-1,-1)|(0,-1)|(1,-1)|(-1,1)

    Transition {
        if cell == alive
            then
                if neighbors(alive) == 3 then alive else dead
            else
                if neighbors(alive) > 3 or neighbors(alive) < 2 then dead else alive
    }
    Initial {
        size 10 10
        Default dead
        
        alive at {}
    }
}

# Colores
En la simulacion, los estados seran representados mediante colores. El usuario debe especificar el color de cada estado.
El programa debe determinar que cada estado tenga un color unico.
Colores disponibles:
    - white | black | lightgray | gray | darkgray 
    - green | red | blue | yellow | orange | violet | rose | magenta | cyan | 




# Gramatica concreta
S -> 'Automaton' IDENT '{' SPECS '}' S

SPECS -> 'States' ':=' STATES 'Neighborhood' ':=' NEIGHBORHOOD  'Transition' '{' RULE '}' 'Initial' '{' INIT_INFO '}'

# ------------------------- estados ------------------------------
STATES -> STATE | STATE '|' STATES
STATE -> IDENT ':' COLOR
IDENT -> string que no empiece con un numero y no tenga caracteres especiales
COLOR -> 'white' | 'black' | ...
# ----------------------------------------------------------------

# ------------------------- vecindad -----------------------------
NEIGHBORHOOD -> LITCOORD | LITCOORD '|' NEIGHBORHOOD
LITCOORD -> '(' INT ',' INT ')'
# ----------------------------------------------------------------

# -------------------- funcion de transicion ---------------------
RULE -> STATEEXP | 'if' BOOLEXP 'then' RULE 'else' RULE | 'case' '{' COND '}' 
COND -> 'otherwise' ':' '{' RULE '}' | BOOLEXP ':' '{' RULE '}' | BOOLEXP ':' '{' RULE '}' COND
# ----------------------------------------------------------------


STATEEXP -> IDENT | 'cell' | 'nei' '(' NAT ')'
NAT -> D | NATD
D -> '0' | ... | '9'

INT -> NAT | '-'NAT

INTEXP -> INT | 'neighbors' '(' STATEEXP ')' | INTEXP '+' INTEXP | INTEXP '-' INTEXP

BOOLEXP -> 'False' | 'True' | BOOLEXP 'and' BOOLEXP | BOOLEXP 'or' BOOLEXP | 'not' BOOLEXP
            | INTEXP '==' INTEXP | INTEXP '<=' INTEXP | INTEXP '<' INTEXP | INTEXP '>=' INTEXP
            | INTEXP '>' INTEXP | INTEXP '!=' INTEXP | STATEEXP '==' STATEEXP
            | STATEEXP '!=' STATEEXP | BELONGS

BELONGS -> STATEEXP 'in' '[' SET ']'
SET -> STATEEXP | STATEEXP ',' SET


cell <- corresponde al estado de la celda a evaluar
nei(k) <- corresponde al estado del k-esimo vecino (segun orden dado en la definicion de vecindad)

