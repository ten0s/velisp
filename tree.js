const antlr4 = require('antlr4');
const AutoLISPLexer = require('./grammar/AutoLISPLexer.js').AutoLISPLexer;
const AutoLISPParser = require('./grammar/AutoLISPParser.js').AutoLISPParser;

const input = '(+ 1 2)';

const chars = new antlr4.InputStream(input);
const lexer = new AutoLISPLexer(chars);

// Don't use JavaScript strictMode
//lexer.strictMode = false;

const tokens = new antlr4.CommonTokenStream(lexer);
const parser = new AutoLISPParser(tokens);
//parser.buildParseTrees = true;
const tree = parser.module();

console.log(tree.toStringTree(parser.ruleNames));
