grammar AutoLISP;

// Parser rules

file : expr+ ;

expr :
     // Special Forms (AutoCAD 2013 AutoLISP Developer's Guild p.37)

       '(' 'and' expr* ')'                                                        # and
     | '(' 'cond' condTestResult* ')'                                             # cond
     | '(' 'defun' defunName '(' defunParam* ( ' / ' defunLocal* )? ')' expr+ ')' # defun
     //                                                                           # foreach
     | '(' 'if' ifTest ifThen ifElse? ')'                                         # if
     //                                                                           # lambda
     | '(' 'or' expr* ')'                                                         # or
     | '(' 'progn' expr* ')'                                                      # progn
     //                                                                           # quote
     | '(' 'repeat' repeatNum expr* ')'                                           # repeat
     | '(' 'setq' setqIdVal+ ')'                                                  # setQ
     | '(' 'while' whileTest expr+ ')'                                            # while

     // Basic Output Functions (AutoCAD 2013 AutoLISP Developer's Guild p.16)

     | '(' 'princ' expr ')'                                                       # princ

     | '(' ID funArg* ')'                                                         # fun

     // Data Types (AutoCAD 2013 AutoLISP Developer's Guild p.6)

     | NIL                                                                        # nil
     | T                                                                          # t
     | INT                                                                        # int
     | REAL                                                                       # real
     | STR                                                                        # str
     | ID                                                                         # id
     | SYM                                                                        # sym
     ;

condTestResult : '(' condTest condResult ')' ;
condTest : expr ;
condResult : expr ;

defunName : ID ;
defunParam : ID ;
defunLocal : ID ;

ifTest : expr ;
ifThen : expr ;
ifElse : expr ;

repeatNum : expr ;

setqIdVal : ID expr ;

whileTest : expr ;

funArg : expr ;

// Lexer rules

NIL : [nN][iI][lL] ;
T : [tT] ;
INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' .*? '"' ;
SYM : '\''ID ;
ID : [a-zA-Z0-9!$%*/\-+=<>~]+ ; // TODO: can't have only numeric chars

INLINE_COMMENT : ';|' .*? '|;' -> skip ; // TODO: inline inside expr doesn't work
LINE_COMMENT : ';'+ .*? NEWLINE -> skip ;

NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : [ \t]+ -> skip ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT  : [0-9] ;
