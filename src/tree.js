import antlr4 from 'antlr4';
import {VeLispLexer} from '../grammar/VeLispLexer.js';
import {VeLispParser} from '../grammar/VeLispParser.js';

const input = '(+ 1 2)';

const chars = new antlr4.InputStream(input);
const lexer = new VeLispLexer(chars);

// Don't use JavaScript strictMode
//lexer.strictMode = false;

const tokens = new antlr4.CommonTokenStream(lexer);
const parser = new VeLispParser(tokens);
//parser.buildParseTrees = true;
const tree = parser.file();

console.log(tree.toStringTree(parser.ruleNames));
