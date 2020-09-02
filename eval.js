const antlr4 = require('antlr4');
const AutoLISPLexer = require('./grammar/AutoLISPLexer.js').AutoLISPLexer;
const AutoLISPParser = require('./grammar/AutoLISPParser.js').AutoLISPParser;
const EvalVisitor = require('./EvalVisitor.js');

//const input = '2';
//const input = '2.0';
//const input = '"2.0"';
//const input = '(list)';
//const input = '(list 1 2 3)';

//const input = '(princ 2)'; // 2
//const input = '(princ 2.0)'; // 2.0
//const input = '(princ "2.0")'; // "2.0" TODO: 2.0?

//const input = '(princ (list))' // () TODO: '() | nil
//const input = '(princ (list 1 2 3))' // (1 2 3)
//const input = '(princ (car (list 1 2 3)))' // 1
//const input = '(princ (cdr (list 1 2 3)))' // (2 3)

//const input = '(princ (*))'; // 1 TODO: 0
//const input = '(princ (* 2))'; // 2
//const input = '(princ (* 2 3))'; // 6
//const input = '(princ (* 2 3.0))'; // 6.0
//const input = '(princ (* 2 3 4.0))'; // 24.0
//const input = '(princ (* 3 -4.5))'; // -13.5
//const input = '(princ (* 1 2 3 4 5))'; // 120

//const input = '(princ (/))' // TODO: expects at least 1 argument, but found none
//const input = '(princ (/ 4))' // 4
//const input = '(princ (/ 5 2))' // 2 Integer division
//const input = '(princ (/ 5 2.0))' // 2.5
//const input = '(princ (/ 100 2))' // 50
//const input = '(princ (/ 100 2.0))' // 50.0
//const input = '(princ (/ 100 20.0 2))' // 2.5
//const input = '(princ (/ 100 20 2))' // 2 Integer division

//const input = '(princ (+))'; // 0
//const input = '(princ (+ 1))'; // 1
//const input = '(princ (+ 1 2))'; // 3
//const input = '(princ (+ 1 2.0))'; // 3.0
//const input = '(princ (+ 2.0 1))'; // 3.0
//const input = '(princ (+ 1 2 3 4 5))'; // 15
//const input = '(princ (+ 2147483646 3))'//2147483649 TODO: -2147483647

//const input = '(princ (-))' // TODO: expects at least 1 argument, but found none
//const input = '(princ (- 1))'; // -1
//const input = '(princ (- 1 2.0))'; // -1.0
//const input = '(princ (- 50 40))'; // 10
//const input = '(princ (- 50 40.0))'; // 10.0
const input = '(princ (- 15 1 2 3 4 5))'; // 0

//const input = '(princ (= 1)'; // true TODO: T
//const input = '(princ (= 4 4.0)'; // true TODO: T
//const input = '(princ (= 20 388)'; // false TODO: nil
//const input = '(princ (= 2.4 2.4 2.4)'; // true TODO: T
//const input = '(princ (= 499 499 500)'; // false TODO: nil
//const input = '(princ (= "me" "me"))'; // true TODO: T

//const input = '(if 1 (princ 1) (princ 0))'; // 1
//const input = '(if 0 (princ 1) (princ 0))'; // 0

//const input = '(princ (= 1 1))'; // true
//const input = '(princ (> 2 1))'; // true
//const input = '(princ (> 1 1))'; // false
//const input = '(princ (if 1 1 0))' // 1
//const input = '(princ (if 0 1 0))' // 0

//const input = '(setq a 1) (princ a) (princ (+ 2 a))'; // 3
//const input = '(setq a 10) (while (> a 0) (princ a) (setq a (- a 1)))';

const chars = new antlr4.InputStream(input);
const lexer = new AutoLISPLexer(chars);

// Don't use JavaScript strictMode
//lexer.strictMode = false;

const tokens = new antlr4.CommonTokenStream(lexer);
const parser = new AutoLISPParser(tokens);
//parser.buildParseTrees = true;
const tree = parser.file();

console.log(tree.toStringTree(parser.ruleNames));

tree.accept(new EvalVisitor());
