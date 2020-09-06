grammar AutoLISP;

// Parser rules

file : expr+ ;

expr :
     // Special Forms (AutoCAD 2013 AutoLISP Developer's Guild p.37)

     // and
     // command
       '(' 'cond' testresult* ')'              # cond
     | '(' 'defun' ID '(' ID* ')' expr+ ')'    # defun // TODO: locals
     // defun-q
     // foreach
     // function
     | '(' 'if' testexpr thenexpr elseexpr? ')' # if
     // lambda
     // or
     // progn
     // quote
     | '(' 'repeat' numexpr expr* ')'          # repeat
     | '(' 'setq' idexpr+ ')'                  # setQ
     // trace
     // untrace
     // vlax-for
     | '(' 'while' testexpr expr+ ')'          # while

     // Basic Output Functions (AutoCAD 2013 AutoLISP Developer's Guild p.16)

     | '(' 'princ' expr ')'                    # princ

     | '(' ID argexpr* ')'                     # funCall // support funexpr

     // Data Types (AutoCAD 2013 AutoLISP Developer's Guild p.6)

     | NIL                                     # nil
     | T                                       # t
     | INT                                     # int
     | REAL                                    # real
     | STR                                     # str
     | ID                                      # id
     ;

testresult : '(' testexpr resultexpr ')'
           ;

resultexpr : expr
           ;

testexpr : expr
         ;

thenexpr : expr
         ;

elseexpr : expr
         ;
         
numexpr : expr
        ;

idexpr : ID expr
       ;

funexpr : expr
        ;

argexpr : expr
        ;

// Lexer rules

NIL : [nN][iI][lL] ;
T : [tT] ;
INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' .*? '"' ;
ID : [a-zA-Z0-9!$%*/\-+=<>~]+ ; // TODO: can't have only numeric chars

INLINE_COMMENT : ';|' .*? '|;' -> skip ; // TODO: inline inside expr doesn't work
LINE_COMMENT : ';'+ .*? NEWLINE -> skip ;

NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : [ \t]+ -> skip ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT  : [0-9] ;

// quote, atom, eq, car, cdr, cons, cond

// quoting protect list from evaluation
// (quote x) == 'x => x
// 'x => x
// (quote (a b c)) => (a b c)

// (atom ()) == (atom nil) == (atom abc) == true (T)
// (atom (a)) == nil | ()

// (lambda (p1 ... pn) expr)

// (null x) is empty list
// (defun null (x) (eq x '()))

// (defun and (x y) (cond (x (cond (y 't) ('t '())) ('t '()))))

// (defun not (x) (cond (x '()) ('t 't)))

// (defun append (x y) (cond ((null x) y) ('t (cons (car x) (append (cdr x) y)))))
