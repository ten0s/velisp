import antlr4 from 'antlr4';
import {VeLispLexer} from './grammar/VeLispLexer.js';
import {VeLispParser} from './grammar/VeLispParser.js';
import {VeLispGlobalContext} from './VeLispContext.js';
import {VeLispEvalVisitor} from './VeLispEvalVisitor.js';

//const input = '(princ 2)'; // 2
//const input = '(princ 2.0)'; // 2.0
//const input = '(princ "2.0")'; // 2.0
//const input = '(princ (= 1 1))'; // T
//const input = '(princ (= 0 1))'; // nil
//const input = '(princ \'foo)'; // foo

const input = `
;(princ (foreach n (list 1 2 3) (princ n)))
;(princ (foreach n (list 1 2 3 4 5) (setq fac (* n fac))))
(princ fac)
`;

export function evaluate(input) {
    const chars = new antlr4.InputStream(input);
    const lexer = new VeLispLexer(chars);
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new VeLispParser(tokens);
    //parser.buildParseTrees = true;
    const tree = parser.file();
    const results = tree.accept(new VeLispEvalVisitor(new VeLispGlobalContext()));
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

evaluate(input);
