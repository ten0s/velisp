// Generated from grammar/VeLisp.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeLispVisitor = require('./VeLispVisitor').VeLispVisitor;

var grammarFileName = "VeLisp.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u001b\u00d5\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010\t\u0010",
    "\u0004\u0011\t\u0011\u0004\u0012\t\u0012\u0003\u0002\u0006\u0002&\n",
    "\u0002\r\u0002\u000e\u0002\'\u0003\u0003\u0003\u0003\u0003\u0003\u0007",
    "\u0003-\n\u0003\f\u0003\u000e\u00030\u000b\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0007\u00036\n\u0003\f\u0003\u000e\u00039\u000b",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003A\n\u0003\f\u0003\u000e\u0003D\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0007\u0003H\n\u0003\f\u0003\u000e\u0003K\u000b\u0003\u0005",
    "\u0003M\n\u0003\u0003\u0003\u0003\u0003\u0006\u0003Q\n\u0003\r\u0003",
    "\u000e\u0003R\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0007\u0003\\\n\u0003\f\u0003\u000e\u0003",
    "_\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0005\u0003h\n\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003o\n\u0003\f\u0003",
    "\u000e\u0003r\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003x\n\u0003\f\u0003\u000e\u0003{\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u0082\n",
    "\u0003\f\u0003\u000e\u0003\u0085\u000b\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u008c\n\u0003\f\u0003",
    "\u000e\u0003\u008f\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0006\u0003\u0096\n\u0003\r\u0003\u000e\u0003\u0097",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u00a4\n",
    "\u0003\f\u0003\u000e\u0003\u00a7\u000b\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0005\u0003\u00b1\n\u0003\u0003\u0004\u0003\u0004\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006\u0003\u0006\u0003",
    "\u0007\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t\u0003\n\u0003\n\u0003",
    "\u000b\u0003\u000b\u0003\f\u0003\f\u0003\r\u0003\r\u0003\u000e\u0003",
    "\u000e\u0003\u000f\u0003\u000f\u0003\u0010\u0003\u0010\u0003\u0010\u0003",
    "\u0011\u0003\u0011\u0003\u0012\u0003\u0012\u0003\u0012\u0002\u0002\u0013",
    "\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c",
    "\u001e \"\u0002\u0002\u0002\u00e4\u0002%\u0003\u0002\u0002\u0002\u0004",
    "\u00b0\u0003\u0002\u0002\u0002\u0006\u00b2\u0003\u0002\u0002\u0002\b",
    "\u00b7\u0003\u0002\u0002\u0002\n\u00b9\u0003\u0002\u0002\u0002\f\u00bb",
    "\u0003\u0002\u0002\u0002\u000e\u00bd\u0003\u0002\u0002\u0002\u0010\u00bf",
    "\u0003\u0002\u0002\u0002\u0012\u00c1\u0003\u0002\u0002\u0002\u0014\u00c3",
    "\u0003\u0002\u0002\u0002\u0016\u00c5\u0003\u0002\u0002\u0002\u0018\u00c7",
    "\u0003\u0002\u0002\u0002\u001a\u00c9\u0003\u0002\u0002\u0002\u001c\u00cb",
    "\u0003\u0002\u0002\u0002\u001e\u00cd\u0003\u0002\u0002\u0002 \u00d0",
    "\u0003\u0002\u0002\u0002\"\u00d2\u0003\u0002\u0002\u0002$&\u0005\u0004",
    "\u0003\u0002%$\u0003\u0002\u0002\u0002&\'\u0003\u0002\u0002\u0002\'",
    "%\u0003\u0002\u0002\u0002\'(\u0003\u0002\u0002\u0002(\u0003\u0003\u0002",
    "\u0002\u0002)*\u0007\u0003\u0002\u0002*.\u0007\u0006\u0002\u0002+-\u0005",
    "\u0004\u0003\u0002,+\u0003\u0002\u0002\u0002-0\u0003\u0002\u0002\u0002",
    ".,\u0003\u0002\u0002\u0002./\u0003\u0002\u0002\u0002/1\u0003\u0002\u0002",
    "\u00020.\u0003\u0002\u0002\u00021\u00b1\u0007\u0004\u0002\u000223\u0007",
    "\u0003\u0002\u000237\u0007\u0007\u0002\u000246\u0005\u0006\u0004\u0002",
    "54\u0003\u0002\u0002\u000269\u0003\u0002\u0002\u000275\u0003\u0002\u0002",
    "\u000278\u0003\u0002\u0002\u00028:\u0003\u0002\u0002\u000297\u0003\u0002",
    "\u0002\u0002:\u00b1\u0007\u0004\u0002\u0002;<\u0007\u0003\u0002\u0002",
    "<=\u0007\b\u0002\u0002=>\u0005\f\u0007\u0002>B\u0007\u0003\u0002\u0002",
    "?A\u0005\u000e\b\u0002@?\u0003\u0002\u0002\u0002AD\u0003\u0002\u0002",
    "\u0002B@\u0003\u0002\u0002\u0002BC\u0003\u0002\u0002\u0002CL\u0003\u0002",
    "\u0002\u0002DB\u0003\u0002\u0002\u0002EI\u0007\u0005\u0002\u0002FH\u0005",
    "\u0010\t\u0002GF\u0003\u0002\u0002\u0002HK\u0003\u0002\u0002\u0002I",
    "G\u0003\u0002\u0002\u0002IJ\u0003\u0002\u0002\u0002JM\u0003\u0002\u0002",
    "\u0002KI\u0003\u0002\u0002\u0002LE\u0003\u0002\u0002\u0002LM\u0003\u0002",
    "\u0002\u0002MN\u0003\u0002\u0002\u0002NP\u0007\u0004\u0002\u0002OQ\u0005",
    "\u0004\u0003\u0002PO\u0003\u0002\u0002\u0002QR\u0003\u0002\u0002\u0002",
    "RP\u0003\u0002\u0002\u0002RS\u0003\u0002\u0002\u0002ST\u0003\u0002\u0002",
    "\u0002TU\u0007\u0004\u0002\u0002U\u00b1\u0003\u0002\u0002\u0002VW\u0007",
    "\u0003\u0002\u0002WX\u0007\t\u0002\u0002XY\u0005\u0012\n\u0002Y]\u0005",
    "\u0014\u000b\u0002Z\\\u0005\u0004\u0003\u0002[Z\u0003\u0002\u0002\u0002",
    "\\_\u0003\u0002\u0002\u0002][\u0003\u0002\u0002\u0002]^\u0003\u0002",
    "\u0002\u0002^`\u0003\u0002\u0002\u0002_]\u0003\u0002\u0002\u0002`a\u0007",
    "\u0004\u0002\u0002a\u00b1\u0003\u0002\u0002\u0002bc\u0007\u0003\u0002",
    "\u0002cd\u0007\n\u0002\u0002de\u0005\u0016\f\u0002eg\u0005\u0018\r\u0002",
    "fh\u0005\u001a\u000e\u0002gf\u0003\u0002\u0002\u0002gh\u0003\u0002\u0002",
    "\u0002hi\u0003\u0002\u0002\u0002ij\u0007\u0004\u0002\u0002j\u00b1\u0003",
    "\u0002\u0002\u0002kl\u0007\u0003\u0002\u0002lp\u0007\u000b\u0002\u0002",
    "mo\u0005\u0004\u0003\u0002nm\u0003\u0002\u0002\u0002or\u0003\u0002\u0002",
    "\u0002pn\u0003\u0002\u0002\u0002pq\u0003\u0002\u0002\u0002qs\u0003\u0002",
    "\u0002\u0002rp\u0003\u0002\u0002\u0002s\u00b1\u0007\u0004\u0002\u0002",
    "tu\u0007\u0003\u0002\u0002uy\u0007\f\u0002\u0002vx\u0005\u0004\u0003",
    "\u0002wv\u0003\u0002\u0002\u0002x{\u0003\u0002\u0002\u0002yw\u0003\u0002",
    "\u0002\u0002yz\u0003\u0002\u0002\u0002z|\u0003\u0002\u0002\u0002{y\u0003",
    "\u0002\u0002\u0002|\u00b1\u0007\u0004\u0002\u0002}~\u0007\u0003\u0002",
    "\u0002~\u007f\u0007\r\u0002\u0002\u007f\u0083\u0005\u001c\u000f\u0002",
    "\u0080\u0082\u0005\u0004\u0003\u0002\u0081\u0080\u0003\u0002\u0002\u0002",
    "\u0082\u0085\u0003\u0002\u0002\u0002\u0083\u0081\u0003\u0002\u0002\u0002",
    "\u0083\u0084\u0003\u0002\u0002\u0002\u0084\u0086\u0003\u0002\u0002\u0002",
    "\u0085\u0083\u0003\u0002\u0002\u0002\u0086\u0087\u0007\u0004\u0002\u0002",
    "\u0087\u00b1\u0003\u0002\u0002\u0002\u0088\u0089\u0007\u0003\u0002\u0002",
    "\u0089\u008d\u0007\u000e\u0002\u0002\u008a\u008c\u0005\u001e\u0010\u0002",
    "\u008b\u008a\u0003\u0002\u0002\u0002\u008c\u008f\u0003\u0002\u0002\u0002",
    "\u008d\u008b\u0003\u0002\u0002\u0002\u008d\u008e\u0003\u0002\u0002\u0002",
    "\u008e\u0090\u0003\u0002\u0002\u0002\u008f\u008d\u0003\u0002\u0002\u0002",
    "\u0090\u00b1\u0007\u0004\u0002\u0002\u0091\u0092\u0007\u0003\u0002\u0002",
    "\u0092\u0093\u0007\u000f\u0002\u0002\u0093\u0095\u0005 \u0011\u0002",
    "\u0094\u0096\u0005\u0004\u0003\u0002\u0095\u0094\u0003\u0002\u0002\u0002",
    "\u0096\u0097\u0003\u0002\u0002\u0002\u0097\u0095\u0003\u0002\u0002\u0002",
    "\u0097\u0098\u0003\u0002\u0002\u0002\u0098\u0099\u0003\u0002\u0002\u0002",
    "\u0099\u009a\u0007\u0004\u0002\u0002\u009a\u00b1\u0003\u0002\u0002\u0002",
    "\u009b\u009c\u0007\u0003\u0002\u0002\u009c\u009d\u0007\u0010\u0002\u0002",
    "\u009d\u009e\u0005\u0004\u0003\u0002\u009e\u009f\u0007\u0004\u0002\u0002",
    "\u009f\u00b1\u0003\u0002\u0002\u0002\u00a0\u00a1\u0007\u0003\u0002\u0002",
    "\u00a1\u00a5\u0007\u0017\u0002\u0002\u00a2\u00a4\u0005\"\u0012\u0002",
    "\u00a3\u00a2\u0003\u0002\u0002\u0002\u00a4\u00a7\u0003\u0002\u0002\u0002",
    "\u00a5\u00a3\u0003\u0002\u0002\u0002\u00a5\u00a6\u0003\u0002\u0002\u0002",
    "\u00a6\u00a8\u0003\u0002\u0002\u0002\u00a7\u00a5\u0003\u0002\u0002\u0002",
    "\u00a8\u00b1\u0007\u0004\u0002\u0002\u00a9\u00b1\u0007\u0011\u0002\u0002",
    "\u00aa\u00b1\u0007\u0012\u0002\u0002\u00ab\u00b1\u0007\u0013\u0002\u0002",
    "\u00ac\u00b1\u0007\u0014\u0002\u0002\u00ad\u00b1\u0007\u0015\u0002\u0002",
    "\u00ae\u00b1\u0007\u0017\u0002\u0002\u00af\u00b1\u0007\u0016\u0002\u0002",
    "\u00b0)\u0003\u0002\u0002\u0002\u00b02\u0003\u0002\u0002\u0002\u00b0",
    ";\u0003\u0002\u0002\u0002\u00b0V\u0003\u0002\u0002\u0002\u00b0b\u0003",
    "\u0002\u0002\u0002\u00b0k\u0003\u0002\u0002\u0002\u00b0t\u0003\u0002",
    "\u0002\u0002\u00b0}\u0003\u0002\u0002\u0002\u00b0\u0088\u0003\u0002",
    "\u0002\u0002\u00b0\u0091\u0003\u0002\u0002\u0002\u00b0\u009b\u0003\u0002",
    "\u0002\u0002\u00b0\u00a0\u0003\u0002\u0002\u0002\u00b0\u00a9\u0003\u0002",
    "\u0002\u0002\u00b0\u00aa\u0003\u0002\u0002\u0002\u00b0\u00ab\u0003\u0002",
    "\u0002\u0002\u00b0\u00ac\u0003\u0002\u0002\u0002\u00b0\u00ad\u0003\u0002",
    "\u0002\u0002\u00b0\u00ae\u0003\u0002\u0002\u0002\u00b0\u00af\u0003\u0002",
    "\u0002\u0002\u00b1\u0005\u0003\u0002\u0002\u0002\u00b2\u00b3\u0007\u0003",
    "\u0002\u0002\u00b3\u00b4\u0005\b\u0005\u0002\u00b4\u00b5\u0005\n\u0006",
    "\u0002\u00b5\u00b6\u0007\u0004\u0002\u0002\u00b6\u0007\u0003\u0002\u0002",
    "\u0002\u00b7\u00b8\u0005\u0004\u0003\u0002\u00b8\t\u0003\u0002\u0002",
    "\u0002\u00b9\u00ba\u0005\u0004\u0003\u0002\u00ba\u000b\u0003\u0002\u0002",
    "\u0002\u00bb\u00bc\u0007\u0017\u0002\u0002\u00bc\r\u0003\u0002\u0002",
    "\u0002\u00bd\u00be\u0007\u0017\u0002\u0002\u00be\u000f\u0003\u0002\u0002",
    "\u0002\u00bf\u00c0\u0007\u0017\u0002\u0002\u00c0\u0011\u0003\u0002\u0002",
    "\u0002\u00c1\u00c2\u0007\u0017\u0002\u0002\u00c2\u0013\u0003\u0002\u0002",
    "\u0002\u00c3\u00c4\u0005\u0004\u0003\u0002\u00c4\u0015\u0003\u0002\u0002",
    "\u0002\u00c5\u00c6\u0005\u0004\u0003\u0002\u00c6\u0017\u0003\u0002\u0002",
    "\u0002\u00c7\u00c8\u0005\u0004\u0003\u0002\u00c8\u0019\u0003\u0002\u0002",
    "\u0002\u00c9\u00ca\u0005\u0004\u0003\u0002\u00ca\u001b\u0003\u0002\u0002",
    "\u0002\u00cb\u00cc\u0005\u0004\u0003\u0002\u00cc\u001d\u0003\u0002\u0002",
    "\u0002\u00cd\u00ce\u0007\u0017\u0002\u0002\u00ce\u00cf\u0005\u0004\u0003",
    "\u0002\u00cf\u001f\u0003\u0002\u0002\u0002\u00d0\u00d1\u0005\u0004\u0003",
    "\u0002\u00d1!\u0003\u0002\u0002\u0002\u00d2\u00d3\u0005\u0004\u0003",
    "\u0002\u00d3#\u0003\u0002\u0002\u0002\u0012\'.7BILR]gpy\u0083\u008d",
    "\u0097\u00a5\u00b0"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'('", "')'", "' / '" ];

var symbolicNames = [ null, null, null, null, "AND", "COND", "DEFUN", "FOREACH", 
                      "IF", "OR", "PROGN", "REPEAT", "SETQ", "WHILE", "PRINC", 
                      "NIL", "TRU", "INT", "REAL", "STR", "SYM", "ID", "INLINE_COMMENT", 
                      "LINE_COMMENT", "NEWLINE", "WHITESPACE" ];

var ruleNames =  [ "file", "expr", "condTestResult", "condTest", "condResult", 
                   "defunName", "defunParam", "defunLocal", "foreachName", 
                   "foreachList", "ifTest", "ifThen", "ifElse", "repeatNum", 
                   "setqNameExpr", "whileTest", "funArg" ];

function VeLispParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

VeLispParser.prototype = Object.create(antlr4.Parser.prototype);
VeLispParser.prototype.constructor = VeLispParser;

Object.defineProperty(VeLispParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

VeLispParser.EOF = antlr4.Token.EOF;
VeLispParser.T__0 = 1;
VeLispParser.T__1 = 2;
VeLispParser.T__2 = 3;
VeLispParser.AND = 4;
VeLispParser.COND = 5;
VeLispParser.DEFUN = 6;
VeLispParser.FOREACH = 7;
VeLispParser.IF = 8;
VeLispParser.OR = 9;
VeLispParser.PROGN = 10;
VeLispParser.REPEAT = 11;
VeLispParser.SETQ = 12;
VeLispParser.WHILE = 13;
VeLispParser.PRINC = 14;
VeLispParser.NIL = 15;
VeLispParser.TRU = 16;
VeLispParser.INT = 17;
VeLispParser.REAL = 18;
VeLispParser.STR = 19;
VeLispParser.SYM = 20;
VeLispParser.ID = 21;
VeLispParser.INLINE_COMMENT = 22;
VeLispParser.LINE_COMMENT = 23;
VeLispParser.NEWLINE = 24;
VeLispParser.WHITESPACE = 25;

VeLispParser.RULE_file = 0;
VeLispParser.RULE_expr = 1;
VeLispParser.RULE_condTestResult = 2;
VeLispParser.RULE_condTest = 3;
VeLispParser.RULE_condResult = 4;
VeLispParser.RULE_defunName = 5;
VeLispParser.RULE_defunParam = 6;
VeLispParser.RULE_defunLocal = 7;
VeLispParser.RULE_foreachName = 8;
VeLispParser.RULE_foreachList = 9;
VeLispParser.RULE_ifTest = 10;
VeLispParser.RULE_ifThen = 11;
VeLispParser.RULE_ifElse = 12;
VeLispParser.RULE_repeatNum = 13;
VeLispParser.RULE_setqNameExpr = 14;
VeLispParser.RULE_whileTest = 15;
VeLispParser.RULE_funArg = 16;


function FileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_file;
    return this;
}

FileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FileContext.prototype.constructor = FileContext;

FileContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

FileContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFile(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FileContext = FileContext;

VeLispParser.prototype.file = function() {

    var localctx = new FileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, VeLispParser.RULE_file);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 35; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 34;
            this.expr();
            this.state = 37; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0));
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_expr;
    return this;
}

ExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExprContext.prototype.constructor = ExprContext;


 
ExprContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};


function OrContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

OrContext.prototype = Object.create(ExprContext.prototype);
OrContext.prototype.constructor = OrContext;

VeLispParser.OrContext = OrContext;

OrContext.prototype.OR = function() {
    return this.getToken(VeLispParser.OR, 0);
};

OrContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
OrContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitOr(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function PrincContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

PrincContext.prototype = Object.create(ExprContext.prototype);
PrincContext.prototype.constructor = PrincContext;

VeLispParser.PrincContext = PrincContext;

PrincContext.prototype.PRINC = function() {
    return this.getToken(VeLispParser.PRINC, 0);
};

PrincContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};
PrincContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitPrinc(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function SymContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

SymContext.prototype = Object.create(ExprContext.prototype);
SymContext.prototype.constructor = SymContext;

VeLispParser.SymContext = SymContext;

SymContext.prototype.SYM = function() {
    return this.getToken(VeLispParser.SYM, 0);
};
SymContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitSym(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function PrognContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

PrognContext.prototype = Object.create(ExprContext.prototype);
PrognContext.prototype.constructor = PrognContext;

VeLispParser.PrognContext = PrognContext;

PrognContext.prototype.PROGN = function() {
    return this.getToken(VeLispParser.PROGN, 0);
};

PrognContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
PrognContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitProgn(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function RealContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

RealContext.prototype = Object.create(ExprContext.prototype);
RealContext.prototype.constructor = RealContext;

VeLispParser.RealContext = RealContext;

RealContext.prototype.REAL = function() {
    return this.getToken(VeLispParser.REAL, 0);
};
RealContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitReal(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function CondContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

CondContext.prototype = Object.create(ExprContext.prototype);
CondContext.prototype.constructor = CondContext;

VeLispParser.CondContext = CondContext;

CondContext.prototype.COND = function() {
    return this.getToken(VeLispParser.COND, 0);
};

CondContext.prototype.condTestResult = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(CondTestResultContext);
    } else {
        return this.getTypedRuleContext(CondTestResultContext,i);
    }
};
CondContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCond(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function WhileContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

WhileContext.prototype = Object.create(ExprContext.prototype);
WhileContext.prototype.constructor = WhileContext;

VeLispParser.WhileContext = WhileContext;

WhileContext.prototype.WHILE = function() {
    return this.getToken(VeLispParser.WHILE, 0);
};

WhileContext.prototype.whileTest = function() {
    return this.getTypedRuleContext(WhileTestContext,0);
};

WhileContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
WhileContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitWhile(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function FunCallContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

FunCallContext.prototype = Object.create(ExprContext.prototype);
FunCallContext.prototype.constructor = FunCallContext;

VeLispParser.FunCallContext = FunCallContext;

FunCallContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

FunCallContext.prototype.funArg = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(FunArgContext);
    } else {
        return this.getTypedRuleContext(FunArgContext,i);
    }
};
FunCallContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFunCall(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function DefunContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DefunContext.prototype = Object.create(ExprContext.prototype);
DefunContext.prototype.constructor = DefunContext;

VeLispParser.DefunContext = DefunContext;

DefunContext.prototype.DEFUN = function() {
    return this.getToken(VeLispParser.DEFUN, 0);
};

DefunContext.prototype.defunName = function() {
    return this.getTypedRuleContext(DefunNameContext,0);
};

DefunContext.prototype.defunParam = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DefunParamContext);
    } else {
        return this.getTypedRuleContext(DefunParamContext,i);
    }
};

DefunContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

DefunContext.prototype.defunLocal = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DefunLocalContext);
    } else {
        return this.getTypedRuleContext(DefunLocalContext,i);
    }
};
DefunContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDefun(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IntContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IntContext.prototype = Object.create(ExprContext.prototype);
IntContext.prototype.constructor = IntContext;

VeLispParser.IntContext = IntContext;

IntContext.prototype.INT = function() {
    return this.getToken(VeLispParser.INT, 0);
};
IntContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitInt(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function NilContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

NilContext.prototype = Object.create(ExprContext.prototype);
NilContext.prototype.constructor = NilContext;

VeLispParser.NilContext = NilContext;

NilContext.prototype.NIL = function() {
    return this.getToken(VeLispParser.NIL, 0);
};
NilContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitNil(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function StrContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

StrContext.prototype = Object.create(ExprContext.prototype);
StrContext.prototype.constructor = StrContext;

VeLispParser.StrContext = StrContext;

StrContext.prototype.STR = function() {
    return this.getToken(VeLispParser.STR, 0);
};
StrContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitStr(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function ForeachContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ForeachContext.prototype = Object.create(ExprContext.prototype);
ForeachContext.prototype.constructor = ForeachContext;

VeLispParser.ForeachContext = ForeachContext;

ForeachContext.prototype.FOREACH = function() {
    return this.getToken(VeLispParser.FOREACH, 0);
};

ForeachContext.prototype.foreachName = function() {
    return this.getTypedRuleContext(ForeachNameContext,0);
};

ForeachContext.prototype.foreachList = function() {
    return this.getTypedRuleContext(ForeachListContext,0);
};

ForeachContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
ForeachContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeach(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function AndContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

AndContext.prototype = Object.create(ExprContext.prototype);
AndContext.prototype.constructor = AndContext;

VeLispParser.AndContext = AndContext;

AndContext.prototype.AND = function() {
    return this.getToken(VeLispParser.AND, 0);
};

AndContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
AndContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitAnd(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function TruContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TruContext.prototype = Object.create(ExprContext.prototype);
TruContext.prototype.constructor = TruContext;

VeLispParser.TruContext = TruContext;

TruContext.prototype.TRU = function() {
    return this.getToken(VeLispParser.TRU, 0);
};
TruContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitTru(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function RepeatContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

RepeatContext.prototype = Object.create(ExprContext.prototype);
RepeatContext.prototype.constructor = RepeatContext;

VeLispParser.RepeatContext = RepeatContext;

RepeatContext.prototype.REPEAT = function() {
    return this.getToken(VeLispParser.REPEAT, 0);
};

RepeatContext.prototype.repeatNum = function() {
    return this.getTypedRuleContext(RepeatNumContext,0);
};

RepeatContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
RepeatContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitRepeat(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function SetQContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

SetQContext.prototype = Object.create(ExprContext.prototype);
SetQContext.prototype.constructor = SetQContext;

VeLispParser.SetQContext = SetQContext;

SetQContext.prototype.SETQ = function() {
    return this.getToken(VeLispParser.SETQ, 0);
};

SetQContext.prototype.setqNameExpr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(SetqNameExprContext);
    } else {
        return this.getTypedRuleContext(SetqNameExprContext,i);
    }
};
SetQContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitSetQ(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IdContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IdContext.prototype = Object.create(ExprContext.prototype);
IdContext.prototype.constructor = IdContext;

VeLispParser.IdContext = IdContext;

IdContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};
IdContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitId(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IfContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IfContext.prototype = Object.create(ExprContext.prototype);
IfContext.prototype.constructor = IfContext;

VeLispParser.IfContext = IfContext;

IfContext.prototype.IF = function() {
    return this.getToken(VeLispParser.IF, 0);
};

IfContext.prototype.ifTest = function() {
    return this.getTypedRuleContext(IfTestContext,0);
};

IfContext.prototype.ifThen = function() {
    return this.getTypedRuleContext(IfThenContext,0);
};

IfContext.prototype.ifElse = function() {
    return this.getTypedRuleContext(IfElseContext,0);
};
IfContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIf(this);
    } else {
        return visitor.visitChildren(this);
    }
};



VeLispParser.ExprContext = ExprContext;

VeLispParser.prototype.expr = function() {

    var localctx = new ExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, VeLispParser.RULE_expr);
    var _la = 0; // Token type
    try {
        this.state = 174;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,15,this._ctx);
        switch(la_) {
        case 1:
            localctx = new AndContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 39;
            this.match(VeLispParser.T__0);
            this.state = 40;
            this.match(VeLispParser.AND);
            this.state = 44;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 41;
                this.expr();
                this.state = 46;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 47;
            this.match(VeLispParser.T__1);
            break;

        case 2:
            localctx = new CondContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 48;
            this.match(VeLispParser.T__0);
            this.state = 49;
            this.match(VeLispParser.COND);
            this.state = 53;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.T__0) {
                this.state = 50;
                this.condTestResult();
                this.state = 55;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 56;
            this.match(VeLispParser.T__1);
            break;

        case 3:
            localctx = new DefunContext(this, localctx);
            this.enterOuterAlt(localctx, 3);
            this.state = 57;
            this.match(VeLispParser.T__0);
            this.state = 58;
            this.match(VeLispParser.DEFUN);
            this.state = 59;
            this.defunName();
            this.state = 60;
            this.match(VeLispParser.T__0);
            this.state = 64;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.ID) {
                this.state = 61;
                this.defunParam();
                this.state = 66;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 74;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeLispParser.T__2) {
                this.state = 67;
                this.match(VeLispParser.T__2);
                this.state = 71;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                while(_la===VeLispParser.ID) {
                    this.state = 68;
                    this.defunLocal();
                    this.state = 73;
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                }
            }

            this.state = 76;
            this.match(VeLispParser.T__1);
            this.state = 78; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 77;
                this.expr();
                this.state = 80; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0));
            this.state = 82;
            this.match(VeLispParser.T__1);
            break;

        case 4:
            localctx = new ForeachContext(this, localctx);
            this.enterOuterAlt(localctx, 4);
            this.state = 84;
            this.match(VeLispParser.T__0);
            this.state = 85;
            this.match(VeLispParser.FOREACH);
            this.state = 86;
            this.foreachName();
            this.state = 87;
            this.foreachList();
            this.state = 91;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 88;
                this.expr();
                this.state = 93;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 94;
            this.match(VeLispParser.T__1);
            break;

        case 5:
            localctx = new IfContext(this, localctx);
            this.enterOuterAlt(localctx, 5);
            this.state = 96;
            this.match(VeLispParser.T__0);
            this.state = 97;
            this.match(VeLispParser.IF);
            this.state = 98;
            this.ifTest();
            this.state = 99;
            this.ifThen();
            this.state = 101;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 100;
                this.ifElse();
            }

            this.state = 103;
            this.match(VeLispParser.T__1);
            break;

        case 6:
            localctx = new OrContext(this, localctx);
            this.enterOuterAlt(localctx, 6);
            this.state = 105;
            this.match(VeLispParser.T__0);
            this.state = 106;
            this.match(VeLispParser.OR);
            this.state = 110;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 107;
                this.expr();
                this.state = 112;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 113;
            this.match(VeLispParser.T__1);
            break;

        case 7:
            localctx = new PrognContext(this, localctx);
            this.enterOuterAlt(localctx, 7);
            this.state = 114;
            this.match(VeLispParser.T__0);
            this.state = 115;
            this.match(VeLispParser.PROGN);
            this.state = 119;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 116;
                this.expr();
                this.state = 121;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 122;
            this.match(VeLispParser.T__1);
            break;

        case 8:
            localctx = new RepeatContext(this, localctx);
            this.enterOuterAlt(localctx, 8);
            this.state = 123;
            this.match(VeLispParser.T__0);
            this.state = 124;
            this.match(VeLispParser.REPEAT);
            this.state = 125;
            this.repeatNum();
            this.state = 129;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 126;
                this.expr();
                this.state = 131;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 132;
            this.match(VeLispParser.T__1);
            break;

        case 9:
            localctx = new SetQContext(this, localctx);
            this.enterOuterAlt(localctx, 9);
            this.state = 134;
            this.match(VeLispParser.T__0);
            this.state = 135;
            this.match(VeLispParser.SETQ);
            this.state = 139;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.ID) {
                this.state = 136;
                this.setqNameExpr();
                this.state = 141;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 142;
            this.match(VeLispParser.T__1);
            break;

        case 10:
            localctx = new WhileContext(this, localctx);
            this.enterOuterAlt(localctx, 10);
            this.state = 143;
            this.match(VeLispParser.T__0);
            this.state = 144;
            this.match(VeLispParser.WHILE);
            this.state = 145;
            this.whileTest();
            this.state = 147; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 146;
                this.expr();
                this.state = 149; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0));
            this.state = 151;
            this.match(VeLispParser.T__1);
            break;

        case 11:
            localctx = new PrincContext(this, localctx);
            this.enterOuterAlt(localctx, 11);
            this.state = 153;
            this.match(VeLispParser.T__0);
            this.state = 154;
            this.match(VeLispParser.PRINC);
            this.state = 155;
            this.expr();
            this.state = 156;
            this.match(VeLispParser.T__1);
            break;

        case 12:
            localctx = new FunCallContext(this, localctx);
            this.enterOuterAlt(localctx, 12);
            this.state = 158;
            this.match(VeLispParser.T__0);
            this.state = 159;
            this.match(VeLispParser.ID);
            this.state = 163;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.SYM) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 160;
                this.funArg();
                this.state = 165;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 166;
            this.match(VeLispParser.T__1);
            break;

        case 13:
            localctx = new NilContext(this, localctx);
            this.enterOuterAlt(localctx, 13);
            this.state = 167;
            this.match(VeLispParser.NIL);
            break;

        case 14:
            localctx = new TruContext(this, localctx);
            this.enterOuterAlt(localctx, 14);
            this.state = 168;
            this.match(VeLispParser.TRU);
            break;

        case 15:
            localctx = new IntContext(this, localctx);
            this.enterOuterAlt(localctx, 15);
            this.state = 169;
            this.match(VeLispParser.INT);
            break;

        case 16:
            localctx = new RealContext(this, localctx);
            this.enterOuterAlt(localctx, 16);
            this.state = 170;
            this.match(VeLispParser.REAL);
            break;

        case 17:
            localctx = new StrContext(this, localctx);
            this.enterOuterAlt(localctx, 17);
            this.state = 171;
            this.match(VeLispParser.STR);
            break;

        case 18:
            localctx = new IdContext(this, localctx);
            this.enterOuterAlt(localctx, 18);
            this.state = 172;
            this.match(VeLispParser.ID);
            break;

        case 19:
            localctx = new SymContext(this, localctx);
            this.enterOuterAlt(localctx, 19);
            this.state = 173;
            this.match(VeLispParser.SYM);
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondTestResultContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condTestResult;
    return this;
}

CondTestResultContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondTestResultContext.prototype.constructor = CondTestResultContext;

CondTestResultContext.prototype.condTest = function() {
    return this.getTypedRuleContext(CondTestContext,0);
};

CondTestResultContext.prototype.condResult = function() {
    return this.getTypedRuleContext(CondResultContext,0);
};

CondTestResultContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondTestResult(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondTestResultContext = CondTestResultContext;

VeLispParser.prototype.condTestResult = function() {

    var localctx = new CondTestResultContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, VeLispParser.RULE_condTestResult);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 176;
        this.match(VeLispParser.T__0);
        this.state = 177;
        this.condTest();
        this.state = 178;
        this.condResult();
        this.state = 179;
        this.match(VeLispParser.T__1);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condTest;
    return this;
}

CondTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondTestContext.prototype.constructor = CondTestContext;

CondTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

CondTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondTestContext = CondTestContext;

VeLispParser.prototype.condTest = function() {

    var localctx = new CondTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, VeLispParser.RULE_condTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 181;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondResultContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condResult;
    return this;
}

CondResultContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondResultContext.prototype.constructor = CondResultContext;

CondResultContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

CondResultContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondResult(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondResultContext = CondResultContext;

VeLispParser.prototype.condResult = function() {

    var localctx = new CondResultContext(this, this._ctx, this.state);
    this.enterRule(localctx, 8, VeLispParser.RULE_condResult);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 183;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function DefunNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_defunName;
    return this;
}

DefunNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DefunNameContext.prototype.constructor = DefunNameContext;

DefunNameContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

DefunNameContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDefunName(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.DefunNameContext = DefunNameContext;

VeLispParser.prototype.defunName = function() {

    var localctx = new DefunNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, VeLispParser.RULE_defunName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 185;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function DefunParamContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_defunParam;
    return this;
}

DefunParamContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DefunParamContext.prototype.constructor = DefunParamContext;

DefunParamContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

DefunParamContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDefunParam(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.DefunParamContext = DefunParamContext;

VeLispParser.prototype.defunParam = function() {

    var localctx = new DefunParamContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, VeLispParser.RULE_defunParam);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 187;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function DefunLocalContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_defunLocal;
    return this;
}

DefunLocalContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DefunLocalContext.prototype.constructor = DefunLocalContext;

DefunLocalContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

DefunLocalContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDefunLocal(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.DefunLocalContext = DefunLocalContext;

VeLispParser.prototype.defunLocal = function() {

    var localctx = new DefunLocalContext(this, this._ctx, this.state);
    this.enterRule(localctx, 14, VeLispParser.RULE_defunLocal);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 189;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ForeachNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_foreachName;
    return this;
}

ForeachNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ForeachNameContext.prototype.constructor = ForeachNameContext;

ForeachNameContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

ForeachNameContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeachName(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.ForeachNameContext = ForeachNameContext;

VeLispParser.prototype.foreachName = function() {

    var localctx = new ForeachNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 16, VeLispParser.RULE_foreachName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 191;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ForeachListContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_foreachList;
    return this;
}

ForeachListContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ForeachListContext.prototype.constructor = ForeachListContext;

ForeachListContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

ForeachListContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeachList(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.ForeachListContext = ForeachListContext;

VeLispParser.prototype.foreachList = function() {

    var localctx = new ForeachListContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, VeLispParser.RULE_foreachList);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 193;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifTest;
    return this;
}

IfTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfTestContext.prototype.constructor = IfTestContext;

IfTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfTestContext = IfTestContext;

VeLispParser.prototype.ifTest = function() {

    var localctx = new IfTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 20, VeLispParser.RULE_ifTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 195;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfThenContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifThen;
    return this;
}

IfThenContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfThenContext.prototype.constructor = IfThenContext;

IfThenContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfThenContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfThen(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfThenContext = IfThenContext;

VeLispParser.prototype.ifThen = function() {

    var localctx = new IfThenContext(this, this._ctx, this.state);
    this.enterRule(localctx, 22, VeLispParser.RULE_ifThen);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 197;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfElseContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifElse;
    return this;
}

IfElseContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfElseContext.prototype.constructor = IfElseContext;

IfElseContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfElseContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfElse(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfElseContext = IfElseContext;

VeLispParser.prototype.ifElse = function() {

    var localctx = new IfElseContext(this, this._ctx, this.state);
    this.enterRule(localctx, 24, VeLispParser.RULE_ifElse);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 199;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function RepeatNumContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_repeatNum;
    return this;
}

RepeatNumContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
RepeatNumContext.prototype.constructor = RepeatNumContext;

RepeatNumContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

RepeatNumContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitRepeatNum(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.RepeatNumContext = RepeatNumContext;

VeLispParser.prototype.repeatNum = function() {

    var localctx = new RepeatNumContext(this, this._ctx, this.state);
    this.enterRule(localctx, 26, VeLispParser.RULE_repeatNum);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 201;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function SetqNameExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_setqNameExpr;
    return this;
}

SetqNameExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
SetqNameExprContext.prototype.constructor = SetqNameExprContext;

SetqNameExprContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

SetqNameExprContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

SetqNameExprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitSetqNameExpr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.SetqNameExprContext = SetqNameExprContext;

VeLispParser.prototype.setqNameExpr = function() {

    var localctx = new SetqNameExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 28, VeLispParser.RULE_setqNameExpr);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 203;
        this.match(VeLispParser.ID);
        this.state = 204;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function WhileTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_whileTest;
    return this;
}

WhileTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
WhileTestContext.prototype.constructor = WhileTestContext;

WhileTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

WhileTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitWhileTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.WhileTestContext = WhileTestContext;

VeLispParser.prototype.whileTest = function() {

    var localctx = new WhileTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 30, VeLispParser.RULE_whileTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 206;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function FunArgContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_funArg;
    return this;
}

FunArgContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunArgContext.prototype.constructor = FunArgContext;

FunArgContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

FunArgContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFunArg(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FunArgContext = FunArgContext;

VeLispParser.prototype.funArg = function() {

    var localctx = new FunArgContext(this, this._ctx, this.state);
    this.enterRule(localctx, 32, VeLispParser.RULE_funArg);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 208;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


exports.VeLispParser = VeLispParser;
