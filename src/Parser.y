{
module Parser where

import           Types
import           Utils(colorToRGBA)
import           Graphics.Gloss.Data.Color
import           Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
}


%monad { P } { thenP } { returnP }
%name parse
%tokentype { Token }
%lexer { lexer } { TEOF }

%token
'Automaton'         { TCA {-cellular automaton-} }
'{'                 { TOCB {-open curled brackets-} }
'}'                 { TCCB {-closed curled brackets-} }
'('                 { TOP {-open parentheses -} }
')'                 { TCP {-closed parentheses -} }
'['                 { TOB {-open brackets-} }
']'                 { TCB {-closed brackets-} }
','                 { Tcomma }
':='                { Tdef }
'States'            { TStates }
'|'                 { Tsep }
'Neighborhood'      { TNeighborhood }
'Transition'        { TTransition }
'if'                { Tif }
'then'              { Tthen } 
'else'              { Telse }
'case'              { Tcase }
'otherwise'         { Totherwise }
':'                 { Tcond }
'Default'           { TDefault }    
'cell'              { Tcell }
'nei'               { Tnei }
'let'               { Tlet }
'='                 { Tassign }
'and'               { Tand }
'or'                { Tor }
'not'               { Tnot }
'in'                { Tin }
'-'                 { Tminus }
'False'             { TFalse }
'True'              { TTrue } 
'=='                { Teq }
'!='                { Tneq }
'<='                { Tle }
'<'                 { Tlesser }
'>='                { Tge }
'>'                 { Tgreater }
'neighbors'         { Tneighbors }
'white'             { Twhite }
'black'             { Tblack }
'gray'              { Tgray }
'red'               { Tred }
'lightred'          { Tlightred }
'blue'              { Tblue }
'lightblue'         { Tlightblue }
'yellow'            { Tyellow }
'darkyellow'        { Tdarkyellow }
'green'             { Tgreen }
'darkgreen'         { Tdarkgreen }
'cyan'              { Tcyan }
'magenta'           { Tmagenta }
'azure'             { Tazure }
'orange'            { Torange }
'rose'              { Trose }
'violet'            { Tviolet }
'#'                 { Thash }
IDENT               { Tid $$ }
NAT                 { Tnat $$ }

%nonassoc 'let'
%left 'or'
%left 'and'
%nonassoc 'not'
%nonassoc 'in'
%nonassoc '==' '!=' '<=' '<' '>=' '>'
%right '-'

%%

Automaton    :: { Automata }  
             : 'Automaton' IDENT '{' 'States' ':=' States
            'Neighborhood' ':=' Neighborhood 'Transition'
            '{' Rule '}' 'Default' ':=' IDENT '}'                      { CA $2 $6 $9 $12 $16 }

States       :: { States }
             : State                                                   { $1 Map.empty }
             | State '|' States                                        { $1 $3 }
   
State        :: { Map.Map State RGBA -> Map.Map State RGBA }
             : IDENT ':' Color                                         { \map -> Map.insert $1 $3 map }

Neighborhood :: { Neighborhood }
             : Litcoord                                                { Vector.singleton $1 }
             | Litcoord '|' Neighborhood                               { Vector.cons $1 $3 }
             
Litcoord     :: { Coord }
             : '(' NAT ',' NAT ')'                                     { Coord (-$4,$2) } 
             | '(' '-' NAT ',' NAT ')'                                 { Coord (-$5,-$3) }   
             | '(' NAT ',' '-' NAT ')'                                 { Coord ($5,$2) }   
             | '(' '-' NAT ',' '-' NAT ')'                             { Coord ($6,-$3) }

Rule         :: { Rule }
             : StateExp                                                { State $1 }
             | 'if' BoolExp 'then' Rule 'else' Rule                    { If $2 $4 $6 }
             | 'case' '{' Cond '}'                                     { $3 }
             | 'let' IDENT '=' IntExp 'in' Rule                        { Let $2 $4 $6 }

Cond         : 'otherwise' ':' '{' Rule '}'                            { $4 }
             | BoolExp ':' '{' Rule '}' Cond                           { If $1 $4 $6 }


StateExp     :: { Exp State }
             : '#' IDENT                                               { Lit $2 }
             | 'cell'                                                  { Self }
             | 'nei' '(' NAT ')'                                       { Neighbor $3 }


IntExp       :: { Exp Int }
             : NAT                                                     { Const $1 }
             | IDENT                                                   { Var $1 }
             | 'neighbors' '(' StateExp ')'                            { Neighbors $3 }
             | '-' IntExp                                              { Opp $2 }


BoolExp      :: { Exp Bool }
             : 'True'                                                  { BTrue }
             | 'False'                                                 { BFalse }
             | BoolExp 'and' BoolExp                                   { And $1 $3 }
             | BoolExp 'or' BoolExp                                    { Or $1 $3 }
             | 'not' BoolExp                                           { Not $2 }
             | IntExp '<' IntExp                                       { Lt $1 $3 }
             | IntExp '<=' IntExp                                      { Le $1 $3 }
             | IntExp '>' IntExp                                       { Gt $1 $3 }
             | IntExp '>=' IntExp                                      { Ge $1 $3 }
             | IntExp '==' IntExp                                      { EqInt $1 $3 }
             | IntExp '!=' IntExp                                      { NeqInt $1 $3 }
             | StateExp '==' StateExp                                  { EqState $1 $3 }
             | StateExp '!=' StateExp                                  { NeqState $1 $3 }
             | StateExp 'in' '[' Set ']'                               { In $1 $4 }

Set          :: { [Exp State] }
             : StateExp                                                { [$1] }
             | StateExp ',' Set                                        { $1 : $3 }


Color        :: { RGBA }
             : 'white'                                                 { colorToRGBA white }
             | 'black'                                                 { colorToRGBA black }
             | 'gray'                                                  { colorToRGBA (greyN 0.5) }
             | 'red'                                                   { colorToRGBA red }
             | 'lightred'                                              { colorToRGBA (light $ light $ light red) }
             | 'blue'                                                  { colorToRGBA blue }
             | 'lightblue'                                             { colorToRGBA (light $ light $ light blue) }
             | 'yellow'                                                { colorToRGBA yellow }
             | 'darkyellow'                                            { colorToRGBA (dark $ dark yellow) }
             | 'green'                                                 { colorToRGBA green }
             | 'darkgreen'                                             { colorToRGBA (dark $ dark green) }
             | 'cyan'                                                  { colorToRGBA cyan }
             | 'magenta'                                               { colorToRGBA magenta }
             | 'azure'                                                 { colorToRGBA azure }
             | 'orange'                                                { colorToRGBA orange }
             | 'rose'                                                  { colorToRGBA rose }
             | 'violet'                                                { colorToRGBA violet }

{
data ParseResult a = Ok a | Failed String  deriving Show

type LineNumber = Int
type ColNumber = Int

type P a = String -> ColNumber -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s c l -> Ok l

returnP :: a -> P a
returnP x = \s c l -> Ok x

thenP :: P a -> (a -> P b) -> P b
thenP p f = \s c l -> case p s c l of
                      Ok x -> f x s c l
                      Failed err -> Failed err 

failP :: String -> P a
failP err = \s c l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP p f = \s c l -> case p s c l of
                        Ok x -> Ok x
                        Failed err -> f err s c l

happyError :: P a
happyError = \s c l -> Failed $ "parse error at line "++(show l) ++ ", column "++(show c)++" ("++ (take 30 s)++"...)"


data Token = TCA
             | TOCB | TCCB
             | TOP | TCP
             | TOB | TCB
             | Tcomma
             | Tdef
             | TStates
             | Tsep
             | TNeighborhood
             | TTransition
             | Tif | Tthen | Telse
             | Tcase | Totherwise | Tcond
             | TDefault
             | Tcell | Tnei
             | Tlet | Tassign
             | Tminus
             | Tand | Tor | Tnot | Tin
             | TFalse | TTrue
             | Teq | Tneq | Tle | Tlesser | Tge | Tgreater
             | Tneighbors
             | Twhite | Tblack | Tgray
             | Tred | Tlightred | Tblue | Tlightblue | Tyellow
             | Tdarkyellow | Tgreen | Tdarkgreen | Tcyan
             | Tmagenta | Tazure | Torange | Trose | Tviolet
             | Thash
             | Tid String
             | Tnat Int
             | TEOF
    deriving Show


lexer :: (Token -> P a) -> P a
lexer cont s = case s of
                [] -> cont TEOF []
                ('\n':cs) -> \c l -> lexer cont cs 1 (l+1)
                (' ':cs) -> \c -> lexer cont cs (c+1)
                (c:cs) | isAlpha c -> lexVar (c:cs)
                       | isDigit c -> lexNat (c:cs)
                (':':'=':cs) -> \c -> cont Tdef cs (c+2)
                ('=':'=':cs) -> \c -> cont Teq cs (c+2)
                ('!':'=':cs) -> \c -> cont Tneq cs (c+2)
                ('<':'=':cs) -> \c -> cont Tle cs (c+2)
                ('>':'=':cs) -> \c -> cont Tge cs (c+2)
                (c:cs) -> case Map.lookup c symbTokens of
                            Just token -> \c -> cont token cs (c+1)
                            Nothing -> \c l -> (failP $
                                             "parse error: unknown character at line "++ show l ++ ", column "++ show c)
                                             "" c l
                     where symbTokens = Map.fromList [ ('{',TOCB)
                                                     , ('}', TCCB)
                                                     , ('(', TOP)
                                                     , (')', TCP)
                                                     , ('[', TOB)
                                                     , (']', TCB)
                                                     , (',', Tcomma)
                                                     , ('|', Tsep)
                                                     , (':', Tcond)
                                                     , ('-', Tminus)
                                                     , ('<', Tlesser)
                                                     , ('>', Tgreater)
                                                     , ('=', Tassign)
                                                     , ('#', Thash) ]

                where lexNat s = let (n,rest) = span isDigit s
                                  in \c -> cont (Tnat (read n)) rest (c + length n)
                      lexVar s = let (word,rest) = span isAlphaNum s
                                  in case Map.lookup word tokens of
                                    Just token -> \c -> cont token rest (c + length word)
                                    Nothing -> \c -> cont (Tid word) rest (c + length word)
                                    where tokens = Map.fromList [ ("Automaton", TCA)
                                                                , ("States", TStates)
                                                                , ("Neighborhood", TNeighborhood)
                                                                , ("Transition", TTransition)
                                                                , ("if", Tif)
                                                                , ("then", Tthen)
                                                                , ("else", Telse)
                                                                , ("case", Tcase)
                                                                , ("otherwise", Totherwise)
                                                                , ("Default", TDefault)
                                                                , ("cell", Tcell)
                                                                , ("nei", Tnei)
                                                                , ("let", Tlet)
                                                                , ("and", Tand)
                                                                , ("or", Tor)
                                                                , ("not", Tnot)
                                                                , ("in", Tin)
                                                                , ("False", TFalse)
                                                                , ("True", TTrue)
                                                                , ("neighbors", Tneighbors)
                                                                , ("white", Twhite)
                                                                , ("black", Tblack)
                                                                , ("gray", Tgray)
                                                                , ("red", Tred)
                                                                , ("lightred", Tlightred)
                                                                , ("blue", Tblue)
                                                                , ("lightblue", Tlightblue)
                                                                , ("yellow", Tyellow)
                                                                , ("darkyellow", Tdarkyellow)
                                                                , ("green", Tgreen)
                                                                , ("darkgreen", Tdarkgreen)
                                                                , ("cyan", Tcyan)
                                                                , ("magenta", Tmagenta)
                                                                , ("azure", Tazure)
                                                                , ("orange", Torange)
                                                                , ("rose", Trose)
                                                                , ("violet", Tviolet)]
}