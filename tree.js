const antlr4 = require('antlr4');
const AutoLispLexer = require('./grammar/AutoLispLexer.js').AutoLispLexer;
const AutoLispParser = require('./grammar/AutoLispParser.js').AutoLispParser;

const input = '(+ 1 2)';

const chars = new antlr4.InputStream(input);
const lexer = new AutoLispLexer(chars);

// Don't use JavaScript strictMode
//lexer.strictMode = false;

const tokens = new antlr4.CommonTokenStream(lexer);
const parser = new AutoLispParser(tokens);
//parser.buildParseTrees = true;
const tree = parser.module();

console.log(tree.toStringTree(parser.ruleNames));
