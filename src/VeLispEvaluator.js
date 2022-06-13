import antlr4 from 'antlr4'
import VeLispLexer from '../grammar/VeLispLexer.js'
import VeLispParser from '../grammar/VeLispParser.js'
import VeLispContext from './VeLispContext.js'
import VeLispContextIniter from './VeLispContextIniter.js'
import VeLispEvalVisitor from './VeLispEvalVisitor.js'
import VeLispErrorListener from './VeLispErrorListener.js'

function evaluate(input, context) {
    input = preprocess(input)
    if (!context) {
        // This case is used for tests
        context = new VeLispContext()
        VeLispContextIniter.initWithKernel(context)
        // Do we really need lib/ included in tests?
        //VeLispContextIniter.initWithLib(context)
    }
    const {tree} = parseInput(input, context)
    const allResults = tree.accept(new VeLispEvalVisitor(context))
    //console.log('allResults:', allResults);
    const result = lastResult(allResults)
    //console.log('result:', result);
    return result
}

function tree(input, context = new VeLispContext()) {
    const {parser, tree} = parseInput(input, context)
    return tree.toStringTree(parser.ruleNames)
}

function parseInput(input, context) {
    const chars = new antlr4.InputStream(input)
    const lexer = new VeLispLexer(chars)
    lexer.removeErrorListeners()
    lexer.addErrorListener(new VeLispErrorListener(context))
    // Don't use JavaScript strictMode
    //lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer)
    const parser = new VeLispParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new VeLispErrorListener(context))
    return {
        lexer,
        tokens,
        parser,
        tree: parser.file(),
    }
}

function preprocess(input) {
    return input
}

function lastResult(res) {
    if (res instanceof Array) {
        return lastResult(res[res.length-1])
    }
    return res
}

export {
    evaluate,
    tree,
}
