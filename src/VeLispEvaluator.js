const antlr4 = require('antlr4');
const {VeLispLexer} = require('../grammar/VeLispLexer.js');
const {VeLispParser} = require('../grammar/VeLispParser.js');
const {GlobalContext} = require('./VeLispContext.js');
const {EvalVisitor} = require('./VeLispEvalVisitor.js');

//const input = '(princ 2)'; // 2
//const input = '(princ 2.0)'; // 2.0
//const input = '(princ "2.0")'; // 2.0
//const input = '(princ (= 1 1))'; // T
//const input = '(princ (= 0 1))'; // nil
//const input = '(princ \'foo)'; // foo
//const input = '(princ)'; // empty

function evaluate(input, context = new GlobalContext()) {
    const chars = new antlr4.InputStream(input);
    const lexer = new VeLispLexer(chars);
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new VeLispParser(tokens);
    //parser.buildParseTrees = true;
    const tree = parser.file();
    const results = tree.accept(new EvalVisitor(context));
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

exports.evaluate = evaluate;
