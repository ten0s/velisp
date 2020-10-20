// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');



var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0002\rx\b\u0001\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010\t\u0010",
    "\u0004\u0011\t\u0011\u0004\u0012\t\u0012\u0003\u0002\u0003\u0002\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0004\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006\u0003",
    "\u0006\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\b\u0003\b\u0003",
    "\b\u0003\b\u0003\b\u0003\b\u0003\b\u0003\b\u0005\bA\n\b\u0003\t\u0006",
    "\tD\n\t\r\t\u000e\tE\u0003\t\u0003\t\u0003\t\u0007\tK\n\t\f\t\u000e",
    "\tN\u000b\t\u0003\n\u0003\n\u0007\nR\n\n\f\n\u000e\nU\u000b\n\u0003",
    "\n\u0003\n\u0003\u000b\u0005\u000bZ\n\u000b\u0003\u000b\u0003\u000b",
    "\u0003\u000b\u0003\u000b\u0003\f\u0006\fa\n\f\r\f\u000e\fb\u0003\f\u0003",
    "\f\u0003\r\u0003\r\u0003\r\u0003\r\u0005\rk\n\r\u0003\u000e\u0003\u000e",
    "\u0003\u000f\u0003\u000f\u0005\u000fq\n\u000f\u0003\u0010\u0003\u0010",
    "\u0003\u0011\u0003\u0011\u0003\u0012\u0003\u0012\u0002\u0002\u0013\u0003",
    "\u0003\u0005\u0004\u0007\u0005\t\u0006\u000b\u0007\r\b\u000f\t\u0011",
    "\n\u0013\u000b\u0015\f\u0017\r\u0019\u0002\u001b\u0002\u001d\u0002\u001f",
    "\u0002!\u0002#\u0002\u0003\u0002\b\u0004\u0002\u000b\u000b\"\"\u0006",
    "\u0002\f\f\u000f\u000f$$^^\u0007\u0002$$^^ppttvv\u0003\u0002c|\u0003",
    "\u0002C\\\u0003\u00022;\u0002|\u0002\u0003\u0003\u0002\u0002\u0002\u0002",
    "\u0005\u0003\u0002\u0002\u0002\u0002\u0007\u0003\u0002\u0002\u0002\u0002",
    "\t\u0003\u0002\u0002\u0002\u0002\u000b\u0003\u0002\u0002\u0002\u0002",
    "\r\u0003\u0002\u0002\u0002\u0002\u000f\u0003\u0002\u0002\u0002\u0002",
    "\u0011\u0003\u0002\u0002\u0002\u0002\u0013\u0003\u0002\u0002\u0002\u0002",
    "\u0015\u0003\u0002\u0002\u0002\u0002\u0017\u0003\u0002\u0002\u0002\u0003",
    "%\u0003\u0002\u0002\u0002\u0005\'\u0003\u0002\u0002\u0002\u0007.\u0003",
    "\u0002\u0002\u0002\t0\u0003\u0002\u0002\u0002\u000b2\u0003\u0002\u0002",
    "\u0002\r4\u0003\u0002\u0002\u0002\u000f@\u0003\u0002\u0002\u0002\u0011",
    "C\u0003\u0002\u0002\u0002\u0013O\u0003\u0002\u0002\u0002\u0015Y\u0003",
    "\u0002\u0002\u0002\u0017`\u0003\u0002\u0002\u0002\u0019j\u0003\u0002",
    "\u0002\u0002\u001bl\u0003\u0002\u0002\u0002\u001dp\u0003\u0002\u0002",
    "\u0002\u001fr\u0003\u0002\u0002\u0002!t\u0003\u0002\u0002\u0002#v\u0003",
    "\u0002\u0002\u0002%&\u0007<\u0002\u0002&\u0004\u0003\u0002\u0002\u0002",
    "\'(\u0007f\u0002\u0002()\u0007k\u0002\u0002)*\u0007c\u0002\u0002*+\u0007",
    "n\u0002\u0002+,\u0007q\u0002\u0002,-\u0007i\u0002\u0002-\u0006\u0003",
    "\u0002\u0002\u0002./\u0007}\u0002\u0002/\b\u0003\u0002\u0002\u00020",
    "1\u0007\u007f\u0002\u00021\n\u0003\u0002\u0002\u000223\u0007=\u0002",
    "\u00023\f\u0003\u0002\u0002\u000245\u0007?\u0002\u00025\u000e\u0003",
    "\u0002\u0002\u000267\u0007v\u0002\u000278\u0007g\u0002\u000289\u0007",
    "z\u0002\u00029A\u0007v\u0002\u0002:;\u0007d\u0002\u0002;<\u0007w\u0002",
    "\u0002<=\u0007v\u0002\u0002=>\u0007v\u0002\u0002>?\u0007q\u0002\u0002",
    "?A\u0007p\u0002\u0002@6\u0003\u0002\u0002\u0002@:\u0003\u0002\u0002",
    "\u0002A\u0010\u0003\u0002\u0002\u0002BD\u0005\u001d\u000f\u0002CB\u0003",
    "\u0002\u0002\u0002DE\u0003\u0002\u0002\u0002EC\u0003\u0002\u0002\u0002",
    "EF\u0003\u0002\u0002\u0002FL\u0003\u0002\u0002\u0002GK\u0005#\u0012",
    "\u0002HK\u0005\u001d\u000f\u0002IK\u0007a\u0002\u0002JG\u0003\u0002",
    "\u0002\u0002JH\u0003\u0002\u0002\u0002JI\u0003\u0002\u0002\u0002KN\u0003",
    "\u0002\u0002\u0002LJ\u0003\u0002\u0002\u0002LM\u0003\u0002\u0002\u0002",
    "M\u0012\u0003\u0002\u0002\u0002NL\u0003\u0002\u0002\u0002OS\u0007$\u0002",
    "\u0002PR\u0005\u0019\r\u0002QP\u0003\u0002\u0002\u0002RU\u0003\u0002",
    "\u0002\u0002SQ\u0003\u0002\u0002\u0002ST\u0003\u0002\u0002\u0002TV\u0003",
    "\u0002\u0002\u0002US\u0003\u0002\u0002\u0002VW\u0007$\u0002\u0002W\u0014",
    "\u0003\u0002\u0002\u0002XZ\u0007\u000f\u0002\u0002YX\u0003\u0002\u0002",
    "\u0002YZ\u0003\u0002\u0002\u0002Z[\u0003\u0002\u0002\u0002[\\\u0007",
    "\f\u0002\u0002\\]\u0003\u0002\u0002\u0002]^\b\u000b\u0002\u0002^\u0016",
    "\u0003\u0002\u0002\u0002_a\t\u0002\u0002\u0002`_\u0003\u0002\u0002\u0002",
    "ab\u0003\u0002\u0002\u0002b`\u0003\u0002\u0002\u0002bc\u0003\u0002\u0002",
    "\u0002cd\u0003\u0002\u0002\u0002de\b\f\u0002\u0002e\u0018\u0003\u0002",
    "\u0002\u0002fk\n\u0003\u0002\u0002gh\u0007^\u0002\u0002hk\u0005\u001b",
    "\u000e\u0002ik\u0005\u0015\u000b\u0002jf\u0003\u0002\u0002\u0002jg\u0003",
    "\u0002\u0002\u0002ji\u0003\u0002\u0002\u0002k\u001a\u0003\u0002\u0002",
    "\u0002lm\t\u0004\u0002\u0002m\u001c\u0003\u0002\u0002\u0002nq\u0005",
    "\u001f\u0010\u0002oq\u0005!\u0011\u0002pn\u0003\u0002\u0002\u0002po",
    "\u0003\u0002\u0002\u0002q\u001e\u0003\u0002\u0002\u0002rs\t\u0005\u0002",
    "\u0002s \u0003\u0002\u0002\u0002tu\t\u0006\u0002\u0002u\"\u0003\u0002",
    "\u0002\u0002vw\t\u0007\u0002\u0002w$\u0003\u0002\u0002\u0002\f\u0002",
    "@EJLSYbjp\u0003\b\u0002\u0002"].join("");


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
VeDclLexer.CONTROL_NAME = 7;
VeDclLexer.ID = 8;
VeDclLexer.STRING = 9;
VeDclLexer.NEWLINE = 10;
VeDclLexer.WHITESPACE = 11;

VeDclLexer.prototype.channelNames = [ "DEFAULT_TOKEN_CHANNEL", "HIDDEN" ];

VeDclLexer.prototype.modeNames = [ "DEFAULT_MODE" ];

VeDclLexer.prototype.literalNames = [ null, "':'", "'dialog'", "'{'", "'}'", 
                                      "';'", "'='" ];

VeDclLexer.prototype.symbolicNames = [ null, null, null, null, null, null, 
                                       null, "CONTROL_NAME", "ID", "STRING", 
                                       "NEWLINE", "WHITESPACE" ];

VeDclLexer.prototype.ruleNames = [ "T__0", "T__1", "T__2", "T__3", "T__4", 
                                   "T__5", "CONTROL_NAME", "ID", "STRING", 
                                   "NEWLINE", "WHITESPACE", "CHAR", "ESCAPE_SEQ", 
                                   "LETTER", "LOWER_LETTER", "UPPER_LETTER", 
                                   "DIGIT" ];

VeDclLexer.prototype.grammarFileName = "VeDcl.g4";


exports.VeDclLexer = VeDclLexer;

