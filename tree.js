import antlr4 from 'antlr4';
import {AutoLISPLexer} from './grammar/AutoLISPLexer.js';
import {AutoLISPParser} from './grammar/AutoLISPParser.js';

const input = '(+ 1 2)';

const chars = new antlr4.InputStream(input);
const lexer = new AutoLISPLexer(chars);

// Don't use JavaScript strictMode
//lexer.strictMode = false;

const tokens = new antlr4.CommonTokenStream(lexer);
const parser = new AutoLISPParser(tokens);
//parser.buildParseTrees = true;
const tree = parser.file();

console.log(tree.toStringTree(parser.ruleNames));
