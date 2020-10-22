// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');



var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0002\u000ez\b\u0001\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004",
    "\u0004\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t",
    "\u0007\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004",
    "\f\t\f\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010",
    "\t\u0010\u0004\u0011\t\u0011\u0004\u0012\t\u0012\u0004\u0013\t\u0013",
    "\u0003\u0002\u0003\u0002\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0004\u0003\u0004\u0003\u0005",
    "\u0003\u0005\u0003\u0006\u0003\u0006\u0003\u0007\u0003\u0007\u0003\u0007",
    "\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\b\u0003\b\u0003\b\u0003",
    "\b\u0003\b\u0003\t\u0003\t\u0003\n\u0006\nF\n\n\r\n\u000e\nG\u0003\n",
    "\u0003\n\u0003\n\u0007\nM\n\n\f\n\u000e\nP\u000b\n\u0003\u000b\u0003",
    "\u000b\u0007\u000bT\n\u000b\f\u000b\u000e\u000bW\u000b\u000b\u0003\u000b",
    "\u0003\u000b\u0003\f\u0005\f\\\n\f\u0003\f\u0003\f\u0003\f\u0003\f\u0003",
    "\r\u0006\rc\n\r\r\r\u000e\rd\u0003\r\u0003\r\u0003\u000e\u0003\u000e",
    "\u0003\u000e\u0003\u000e\u0005\u000em\n\u000e\u0003\u000f\u0003\u000f",
    "\u0003\u0010\u0003\u0010\u0005\u0010s\n\u0010\u0003\u0011\u0003\u0011",
    "\u0003\u0012\u0003\u0012\u0003\u0013\u0003\u0013\u0002\u0002\u0014\u0003",
    "\u0003\u0005\u0004\u0007\u0005\t\u0006\u000b\u0007\r\b\u000f\t\u0011",
    "\n\u0013\u000b\u0015\f\u0017\r\u0019\u000e\u001b\u0002\u001d\u0002\u001f",
    "\u0002!\u0002#\u0002%\u0002\u0003\u0002\b\u0004\u0002\u000b\u000b\"",
    "\"\u0006\u0002\f\f\u000f\u000f$$^^\u0007\u0002$$^^ppttvv\u0003\u0002",
    "c|\u0003\u0002C\\\u0003\u00022;\u0002}\u0002\u0003\u0003\u0002\u0002",
    "\u0002\u0002\u0005\u0003\u0002\u0002\u0002\u0002\u0007\u0003\u0002\u0002",
    "\u0002\u0002\t\u0003\u0002\u0002\u0002\u0002\u000b\u0003\u0002\u0002",
    "\u0002\u0002\r\u0003\u0002\u0002\u0002\u0002\u000f\u0003\u0002\u0002",
    "\u0002\u0002\u0011\u0003\u0002\u0002\u0002\u0002\u0013\u0003\u0002\u0002",
    "\u0002\u0002\u0015\u0003\u0002\u0002\u0002\u0002\u0017\u0003\u0002\u0002",
    "\u0002\u0002\u0019\u0003\u0002\u0002\u0002\u0003\'\u0003\u0002\u0002",
    "\u0002\u0005)\u0003\u0002\u0002\u0002\u00070\u0003\u0002\u0002\u0002",
    "\t2\u0003\u0002\u0002\u0002\u000b4\u0003\u0002\u0002\u0002\r6\u0003",
    "\u0002\u0002\u0002\u000f;\u0003\u0002\u0002\u0002\u0011B\u0003\u0002",
    "\u0002\u0002\u0013E\u0003\u0002\u0002\u0002\u0015Q\u0003\u0002\u0002",
    "\u0002\u0017[\u0003\u0002\u0002\u0002\u0019b\u0003\u0002\u0002\u0002",
    "\u001bl\u0003\u0002\u0002\u0002\u001dn\u0003\u0002\u0002\u0002\u001f",
    "r\u0003\u0002\u0002\u0002!t\u0003\u0002\u0002\u0002#v\u0003\u0002\u0002",
    "\u0002%x\u0003\u0002\u0002\u0002\'(\u0007<\u0002\u0002(\u0004\u0003",
    "\u0002\u0002\u0002)*\u0007f\u0002\u0002*+\u0007k\u0002\u0002+,\u0007",
    "c\u0002\u0002,-\u0007n\u0002\u0002-.\u0007q\u0002\u0002./\u0007i\u0002",
    "\u0002/\u0006\u0003\u0002\u0002\u000201\u0007}\u0002\u00021\b\u0003",
    "\u0002\u0002\u000223\u0007\u007f\u0002\u00023\n\u0003\u0002\u0002\u0002",
    "45\u0007=\u0002\u00025\f\u0003\u0002\u0002\u000267\u0007v\u0002\u0002",
    "78\u0007g\u0002\u000289\u0007z\u0002\u00029:\u0007v\u0002\u0002:\u000e",
    "\u0003\u0002\u0002\u0002;<\u0007d\u0002\u0002<=\u0007w\u0002\u0002=",
    ">\u0007v\u0002\u0002>?\u0007v\u0002\u0002?@\u0007q\u0002\u0002@A\u0007",
    "p\u0002\u0002A\u0010\u0003\u0002\u0002\u0002BC\u0007?\u0002\u0002C\u0012",
    "\u0003\u0002\u0002\u0002DF\u0005\u001f\u0010\u0002ED\u0003\u0002\u0002",
    "\u0002FG\u0003\u0002\u0002\u0002GE\u0003\u0002\u0002\u0002GH\u0003\u0002",
    "\u0002\u0002HN\u0003\u0002\u0002\u0002IM\u0005%\u0013\u0002JM\u0005",
    "\u001f\u0010\u0002KM\u0007a\u0002\u0002LI\u0003\u0002\u0002\u0002LJ",
    "\u0003\u0002\u0002\u0002LK\u0003\u0002\u0002\u0002MP\u0003\u0002\u0002",
    "\u0002NL\u0003\u0002\u0002\u0002NO\u0003\u0002\u0002\u0002O\u0014\u0003",
    "\u0002\u0002\u0002PN\u0003\u0002\u0002\u0002QU\u0007$\u0002\u0002RT",
    "\u0005\u001b\u000e\u0002SR\u0003\u0002\u0002\u0002TW\u0003\u0002\u0002",
    "\u0002US\u0003\u0002\u0002\u0002UV\u0003\u0002\u0002\u0002VX\u0003\u0002",
    "\u0002\u0002WU\u0003\u0002\u0002\u0002XY\u0007$\u0002\u0002Y\u0016\u0003",
    "\u0002\u0002\u0002Z\\\u0007\u000f\u0002\u0002[Z\u0003\u0002\u0002\u0002",
    "[\\\u0003\u0002\u0002\u0002\\]\u0003\u0002\u0002\u0002]^\u0007\f\u0002",
    "\u0002^_\u0003\u0002\u0002\u0002_`\b\f\u0002\u0002`\u0018\u0003\u0002",
    "\u0002\u0002ac\t\u0002\u0002\u0002ba\u0003\u0002\u0002\u0002cd\u0003",
    "\u0002\u0002\u0002db\u0003\u0002\u0002\u0002de\u0003\u0002\u0002\u0002",
    "ef\u0003\u0002\u0002\u0002fg\b\r\u0002\u0002g\u001a\u0003\u0002\u0002",
    "\u0002hm\n\u0003\u0002\u0002ij\u0007^\u0002\u0002jm\u0005\u001d\u000f",
    "\u0002km\u0005\u0017\f\u0002lh\u0003\u0002\u0002\u0002li\u0003\u0002",
    "\u0002\u0002lk\u0003\u0002\u0002\u0002m\u001c\u0003\u0002\u0002\u0002",
    "no\t\u0004\u0002\u0002o\u001e\u0003\u0002\u0002\u0002ps\u0005!\u0011",
    "\u0002qs\u0005#\u0012\u0002rp\u0003\u0002\u0002\u0002rq\u0003\u0002",
    "\u0002\u0002s \u0003\u0002\u0002\u0002tu\t\u0005\u0002\u0002u\"\u0003",
    "\u0002\u0002\u0002vw\t\u0006\u0002\u0002w$\u0003\u0002\u0002\u0002x",
    "y\t\u0007\u0002\u0002y&\u0003\u0002\u0002\u0002\u000b\u0002GLNU[dlr",
    "\u0003\b\u0002\u0002"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

function VeDclLexer(input) {
	antlr4.Lexer.call(this, input);
    this._interp = new antlr4.atn.LexerATNSimulator(this, atn, decisionsToDFA, new antlr4.PredictionContextCache());
    return this;
}

VeDclLexer.prototype = Object.create(antlr4.Lexer.prototype);
VeDclLexer.prototype.constructor = VeDclLexer;

Object.defineProperty(VeDclLexer.prototype, "atn", {
        get : function() {
                return atn;
        }
});

VeDclLexer.EOF = antlr4.Token.EOF;
VeDclLexer.T__0 = 1;
VeDclLexer.T__1 = 2;
VeDclLexer.T__2 = 3;
VeDclLexer.T__3 = 4;
VeDclLexer.T__4 = 5;
VeDclLexer.T__5 = 6;
VeDclLexer.T__6 = 7;
VeDclLexer.T__7 = 8;
VeDclLexer.ID = 9;
VeDclLexer.STRING = 10;
VeDclLexer.NEWLINE = 11;
VeDclLexer.WHITESPACE = 12;

VeDclLexer.prototype.channelNames = [ "DEFAULT_TOKEN_CHANNEL", "HIDDEN" ];

VeDclLexer.prototype.modeNames = [ "DEFAULT_MODE" ];

VeDclLexer.prototype.literalNames = [ null, "':'", "'dialog'", "'{'", "'}'", 
                                      "';'", "'text'", "'button'", "'='" ];

VeDclLexer.prototype.symbolicNames = [ null, null, null, null, null, null, 
                                       null, null, null, "ID", "STRING", 
                                       "NEWLINE", "WHITESPACE" ];

VeDclLexer.prototype.ruleNames = [ "T__0", "T__1", "T__2", "T__3", "T__4", 
                                   "T__5", "T__6", "T__7", "ID", "STRING", 
                                   "NEWLINE", "WHITESPACE", "CHAR", "ESCAPE_SEQ", 
                                   "LETTER", "LOWER_LETTER", "UPPER_LETTER", 
                                   "DIGIT" ];

VeDclLexer.prototype.grammarFileName = "VeDcl.g4";


exports.VeDclLexer = VeDclLexer;

