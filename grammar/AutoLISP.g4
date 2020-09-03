grammar AutoLISP;

// Parser rules

file : expr+ ;

expr :
     // Operators (AutoCAD 2013 AutoLISP Reference Guild p.1)
       
       '(' '*' expr* ')'                       # multiply
     | '(' '/' expr* ')'                       # divide
     | '(' '+' expr* ')'                       # add
     | '(' '-' expr* ')'                       # subtract
     | '(' '=' expr+ ')'                       # equalTo
     | '(' '/=' expr+ ')'                      # notEqualTo
     | '(' '<' expr+ ')'                       # lessThan
     | '(' '<=' expr+ ')'                      # lessThanOrEqualTo
     | '(' '>' expr+ ')'                       # greaterThan
     | '(' '>=' expr+ ')'                      # greaterThanOrEqualTo
     | '(' '~' expr ')'                        # bitwiseNOT
     | '(' '1+' expr ')'                       # increment // re-impl in lisp
     | '(' '1-' expr ')'                       # decrement // re-impl in lisp

     | '(' 'list' expr* ')'                    # list 
     | '(' 'car' expr ')'                      # car // TODO: is expr correct here?
     | '(' 'cdr' expr ')'                      # cdr // TODO: is expr correct here?

     // Special Forms (AutoCAD 2013 AutoLISP Developer's Guild p.37)

     // and
     // command
     | '(' 'cond' test+ ')'                    # cond
     | '(' 'defun' SYMBOL '(' SYMBOL* ')' expr+ ')'    # defun
     // defun-q
     // foreach
     // function
     | '(' 'if' testexpr thenexpr elseexpr? ')' # if
     // lambda
     // or
     // progn
     // quote
     // repeat
     | '(' 'setq' SYMBOL expr ')'              # setQ // TODO: multiple 
     // trace
     // untrace
     // vlax-for
     | '(' 'while' testexpr expr+ ')'          # while

     // Basic Output Functions (AutoCAD 2013 AutoLISP Developer's Guild p.16)

     | '(' 'princ' expr ')'                    # princ

     // Data Types (AutoCAD 2013 AutoLISP Developer's Guild p.6)

     | NIL                                     # nil
     | T                                       # t
     | INT                                     # int
     | REAL                                    # real
     | STR                                     # str
     | VAR                                     # var
     ;

testexpr : expr
         ;

thenexpr : expr
         ;

elseexpr : expr
         ;
         
test : (expr expr)
     ;

// Lexer rules

NIL : [nN][iI][lL] ;
T : [tT] ;
INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' .*? '"' ;
VAR : ID ;
SYMBOL : ID ;

WHITESPACE: [ \r\n]+ -> skip ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT  : [0-9] ;
fragment ID     : LETTER[a-zA-Z0-9_]* ;

// (operator arguments*)

// primitive operators
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
