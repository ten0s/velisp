grammar VeLisp;

// Parser rules

file : expr* ;

expr :
       '(' AND expr* ')'                                                          # and
     | '(' COND condTestResult* ')'                                               # cond
     | '(' DEFUN funName '(' funParam* ( ' / ' funLocal* )? ')' expr+ ')'         # defun
     | '(' FOREACH foreachName foreachList expr* ')'                              # foreach
     | '(' IF ifTest ifThen ifElse? ')'                                           # if
     | '(' LAMBDA '(' funParam* ( ' / ' funLocal* )? ')' expr+ ')'                # lambda
     | '(' OR expr* ')'                                                           # or
     | '(' PROGN expr* ')'                                                        # progn
     //                                                                           # quote
     | '(' REPEAT repeatNum expr* ')'                                             # repeat
     | '(' SETQ setqNameExpr* ')'                                                 # setQ
     | '(' WHILE whileTest expr+ ')'                                              # while

     | '(' ID funArg* ')'                                                         # funCall

     | NIL                                                                        # nil
     | TRU                                                                        # tru
     | INT                                                                        # int
     | REAL                                                                       # real
     | STR                                                                        # str
     | ID                                                                         # id
     | SYM                                                                        # sym
     ;

condTestResult : '(' condTest condResult* ')' ;
condTest : expr ;
condResult : expr ;

funName : ID ;
funParam : ID ;
funLocal : ID ;

foreachName : ID ;
foreachList : expr ;

ifTest : expr ;
ifThen : expr ;
ifElse : expr ;

repeatNum : expr ;

setqNameExpr : ID expr ;

whileTest : expr ;

funArg : expr ;

// Lexer rules

AND : A N D ;
COND : C O N D ;
DEFUN : D E F U N ;
FOREACH : F O R E A C H ;
IF : I F ;
LAMBDA : L A M B D A ;
OR : O R ;
PROGN : P R O G N ;
REPEAT : R E P E A T ;
SETQ : S E T Q ;
WHILE : W H I L E ;
NIL : N I L ;
TRU : T ;

INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' CHAR* '"' ;
SYM : '\''ID ;
ID : [a-zA-Z0-9!$%*/\-+=<>~:]+ ; // TODO: can't have only numeric chars

INLINE_COMMENT : ';|' .*? '|;' -> skip ; // TODO: inline inside expr doesn't work
LINE_COMMENT : ';'+ .*? NEWLINE -> skip ;

NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : [ \t]+ -> skip ;

fragment CHAR
         : ~["\\\r\n]
         | '\\' ESCAPE_SEQ
         | NEWLINE
         ;

fragment ESCAPE_SEQ
         : ["\\rnt]
         ;

fragment A : [aA] ;
fragment B : [bB] ;
fragment C : [cC] ;
fragment D : [dD] ;
fragment E : [eE] ;
fragment F : [fF] ;
fragment G : [gG] ;
fragment H : [hH] ;
fragment I : [iI] ;
fragment J : [jJ] ;
fragment K : [kK] ;
fragment L : [lL] ;
fragment M : [mM] ;
fragment N : [nN] ;
fragment O : [oO] ;
fragment P : [pP] ;
fragment Q : [qQ] ;
fragment R : [rR] ;
fragment S : [sS] ;
fragment T : [tT] ;
fragment U : [uU] ;
fragment V : [vV] ;
fragment W : [wW] ;
fragment X : [xX] ;
fragment Y : [yY] ;
fragment Z : [zZ] ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT  : [0-9] ;
