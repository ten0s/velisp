import antlr4 from 'antlr4';
import {AutoLISPLexer} from './grammar/AutoLISPLexer.js';
import {AutoLISPParser} from './grammar/AutoLISPParser.js';
import {EvalVisitor} from './EvalVisitor.js';

//const input = '(princ 2)'; // 2
//const input = '(princ 2.0)'; // 2.0
//const input = '(princ "2.0")'; // 2.0
//const input = '(princ (= 1 1))'; // T
//const input = '(princ (= 0 1))'; // nil

const input = '(princ (= 1))';

//const input = '(princ (> 2 1))'; // T
//const input = '(princ (> 1 1))'; // nil

//const input = '(if 1 (princ 1) (princ 0))'; // 1
//const input = '(if 0 (princ 1) (princ 0))'; // 0
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
