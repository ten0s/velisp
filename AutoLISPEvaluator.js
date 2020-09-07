import antlr4 from 'antlr4';
import {AutoLISPLexer} from './grammar/AutoLISPLexer.js';
import {AutoLISPParser} from './grammar/AutoLISPParser.js';
import {AutoLISPGlobalContext} from './AutoLISPContext.js';
import {AutoLISPEvalVisitor} from './AutoLISPEvalVisitor.js';

//const input = '(princ 2)'; // 2
//const input = '(princ 2.0)'; // 2.0
//const input = '(princ "2.0")'; // 2.0
//const input = '(princ (= 1 1))'; // T
//const input = '(princ (= 0 1))'; // nil

const input = `
(defun fib (n)
  (fib-iter 0 1 n))

(defun fib-iter (a b counter)
  (if (= counter 0)
    a
    (fib-iter b (+ a b) (- counter 1))))

(princ (fib 10))
`;

export function evaluate(input) {
    const chars = new antlr4.InputStream(input);
    const lexer = new AutoLISPLexer(chars);
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new AutoLISPParser(tokens);
    //parser.buildParseTrees = true;
    const tree = parser.file();
    const results = tree.accept(new AutoLISPEvalVisitor(new AutoLISPGlobalContext()));
    //console.log(results);
    const result = getResult(results);
    //console.log(result);
    return result;
}

function getResult(res) {
    if (res instanceof Array) {
        return getResult(res[res.length-1]);
    } else {
        return res;
    }
}

//evaluate(input);
