const antlr4 = require('antlr4');
const {VeLispLexer} = require('../grammar/VeLispLexer.js');
const {VeLispParser} = require('../grammar/VeLispParser.js');
const {VeLispGlobalContext} = require('./VeLispGlobalContext.js');
const {EvalVisitor} = require('./VeLispEvalVisitor.js');

function evaluate(input, context = new VeLispGlobalContext()) {
    input = preprocess(input);
    const {tree} = parseInput(input);
    const allResults = tree.accept(new EvalVisitor(context));
    //console.log('allResults:', allResults);
    const result = lastResult(allResults);
    //console.log('result:', result);
    return result;
}

function tree(input, context = null) {
    const {parser, tree} = parseInput(input);
    return tree.toStringTree(parser.ruleNames);
}

function parseInput(input) {
    const chars = new antlr4.InputStream(input);
    const lexer = new VeLispLexer(chars);
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new VeLispParser(tokens);
    //parser.buildParseTrees = true;
    return {
        lexer,
        tokens,
        parser,
        tree: parser.file(),
    };
}

function preprocess(input) {
    return input;
}

function lastResult(res) {
    if (res instanceof Array) {
        return lastResult(res[res.length-1]);
    }
    return res;
}

exports.evaluate = evaluate;
exports.tree = tree;
