import antlr4 from 'antlr4';
import {AutoLISPLexer} from './grammar/AutoLISPLexer.js';
import {AutoLISPParser} from './grammar/AutoLISPParser.js';
import {EvalVisitor} from './EvalVisitor.js';

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

export function evaluate(input) {
    const chars = new antlr4.InputStream(input);
    const lexer = new AutoLISPLexer(chars);
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new AutoLISPParser(tokens);
    //parser.buildParseTrees = true;
    const tree = parser.file();
    return getValue(tree.accept(new EvalVisitor()));
}

function getValue(expr) {
    if (expr instanceof Array) {
        return getValue(expr[0]);
    }
    return expr;
}

//evaluate(input);
