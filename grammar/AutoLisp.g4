grammar AutoLisp;

// Parser rules

module : expr+ ;

expr :
     // Operators
       
       '(' '*' expr* ')'                       # multiply
     | '(' '/' expr+ ')'                       # divide
     | '(' '+' expr* ')'                       # add
     | '(' '-' expr+ ')'                       # subtract
     | '(' '=' expr+ ')'                       # equalTo
     | '(' '/=' expr+ ')'                      # notEqualTo
     | '(' '<' expr+ ')'                       # lessThan
     | '(' '<=' expr+ ')'                      # lessThanOrEqualTo
     | '(' '>' expr+ ')'                       # greaterThan
     | '(' '<=' expr+ ')'                      # greaterThanOrEqualTo
     | '(' '~' expr ')'                        # bitwiseNOT
     | '(' '1+' expr ')'                       # increment // re-impl in lisp
     | '(' '1-' expr ')'                       # decrement // re-impl in lisp

     // Special forms

     | '(' 'list' expr* ')'                    # list
     | '(' 'setq' ID expr ')'                  # setQ
     | '(' 'if' testexpr thenexpr elseexpr ')' # if    // (re-)impl in lisp using cond
     | '(' 'while' testexpr expr+ ')'          # while
     
     | '(' 'defun' ID '(' ID* ')' expr+ ')'    # defun
     | '(' 'cond' test+ ')'                    # cond

     | '(' 'princ' expr ')'                    # princ

     | ID                                      # id // VAR?
     | INTEGER                                 # integer
     | REAL                                    # real
     | STRING                                  # string
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

ID : LETTER[a-zA-Z0-9_]* ;
INTEGER : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STRING : '"' .*? '"' ;
WHITESPACE: [ \r\n]+ -> skip ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT : [0-9] ;

/*

file : expr+ ;

expr : ATOM
     | list
     ;

list : '(' expr* ')' ;

NIL : 'nil' ;

ATOM : [a-z]+ ;
*/

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
