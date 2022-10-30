/* File parser.mly */
%{open Asyntax%}
%token <int> INT
%token <float> FLOAT
%token PLUS MINUS TIMES DIV PLUSF MINUSF TIMESF MOD INTTYPE FLOATTYPE
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS PLUSF MINUSF      /* lowest precedence */
%left TIMES TIMESF DIV MOD INTTYPE FLOATTYPE     /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Asyntax.asyn> main
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { Int($1) }
    | FLOAT                   { Float($1) }
    | INTTYPE LPAREN expr RPAREN { Inttype($3) }
    | FLOATTYPE LPAREN expr RPAREN { Floattype($3) }
    | LPAREN expr RPAREN      { $2 }
    | MINUS LPAREN expr RPAREN { Sub(Int(0), $3) }
    | expr PLUS expr          { Add($1, $3) }
    | expr MINUS expr         { Sub($1, $3) }
    | expr TIMES expr         { Mul($1, $3) }
    | expr DIV expr           { Div($1, $3) }
    | expr PLUSF expr         { Addf($1, $3) }
    | expr MINUSF expr        { Subf($1, $3) }
    | expr TIMESF expr        { Mulf($1, $3) }
    | expr MOD expr           { Mod($1, $3) }
;