// Generated from grammar/VeLisp.g4 by ANTLR 4.10.1
// jshint ignore: start
import antlr4 from 'antlr4';
import VeLispVisitor from './VeLispVisitor.js';

const serializedATN = [4,1,28,259,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,
4,2,5,7,5,2,6,7,6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,
2,13,7,13,2,14,7,14,2,15,7,15,2,16,7,16,1,0,5,0,36,8,0,10,0,12,0,39,9,0,
1,1,1,1,1,1,5,1,44,8,1,10,1,12,1,47,9,1,1,1,1,1,1,1,1,1,5,1,53,8,1,10,1,
12,1,56,9,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,64,8,1,10,1,12,1,67,9,1,1,1,1,1,
5,1,71,8,1,10,1,12,1,74,9,1,3,1,76,8,1,1,1,1,1,4,1,80,8,1,11,1,12,1,81,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,91,8,1,10,1,12,1,94,9,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,108,8,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,116,
8,1,10,1,12,1,119,9,1,1,1,1,1,5,1,123,8,1,10,1,12,1,126,9,1,3,1,128,8,1,
1,1,1,1,4,1,132,8,1,11,1,12,1,133,1,1,1,1,1,1,1,1,1,1,5,1,141,8,1,10,1,12,
1,144,9,1,1,1,1,1,1,1,1,1,5,1,150,8,1,10,1,12,1,153,9,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,5,1,165,8,1,10,1,12,1,168,9,1,1,1,1,1,1,1,1,1,1,
1,5,1,175,8,1,10,1,12,1,178,9,1,1,1,1,1,1,1,1,1,1,1,5,1,185,8,1,10,1,12,
1,188,9,1,1,1,1,1,1,1,1,1,4,1,194,8,1,11,1,12,1,195,1,1,1,1,1,1,1,1,1,1,
1,1,5,1,204,8,1,10,1,12,1,207,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,
1,218,8,1,1,2,1,2,1,2,5,2,223,8,2,10,2,12,2,226,9,2,1,2,1,2,1,3,1,3,1,4,
1,4,1,5,1,5,1,6,1,6,1,7,1,7,1,8,1,8,1,9,1,9,1,10,1,10,1,11,1,11,1,12,1,12,
1,13,1,13,1,14,1,14,1,14,1,15,1,15,1,16,1,16,1,16,0,0,17,0,2,4,6,8,10,12,
14,16,18,20,22,24,26,28,30,32,0,0,283,0,37,1,0,0,0,2,217,1,0,0,0,4,219,1,
0,0,0,6,229,1,0,0,0,8,231,1,0,0,0,10,233,1,0,0,0,12,235,1,0,0,0,14,237,1,
0,0,0,16,239,1,0,0,0,18,241,1,0,0,0,20,243,1,0,0,0,22,245,1,0,0,0,24,247,
1,0,0,0,26,249,1,0,0,0,28,251,1,0,0,0,30,254,1,0,0,0,32,256,1,0,0,0,34,36,
3,2,1,0,35,34,1,0,0,0,36,39,1,0,0,0,37,35,1,0,0,0,37,38,1,0,0,0,38,1,1,0,
0,0,39,37,1,0,0,0,40,41,5,1,0,0,41,45,5,6,0,0,42,44,3,2,1,0,43,42,1,0,0,
0,44,47,1,0,0,0,45,43,1,0,0,0,45,46,1,0,0,0,46,48,1,0,0,0,47,45,1,0,0,0,
48,218,5,2,0,0,49,50,5,1,0,0,50,54,5,7,0,0,51,53,3,4,2,0,52,51,1,0,0,0,53,
56,1,0,0,0,54,52,1,0,0,0,54,55,1,0,0,0,55,57,1,0,0,0,56,54,1,0,0,0,57,218,
5,2,0,0,58,59,5,1,0,0,59,60,5,8,0,0,60,61,3,10,5,0,61,65,5,1,0,0,62,64,3,
12,6,0,63,62,1,0,0,0,64,67,1,0,0,0,65,63,1,0,0,0,65,66,1,0,0,0,66,75,1,0,
0,0,67,65,1,0,0,0,68,72,5,3,0,0,69,71,3,14,7,0,70,69,1,0,0,0,71,74,1,0,0,
0,72,70,1,0,0,0,72,73,1,0,0,0,73,76,1,0,0,0,74,72,1,0,0,0,75,68,1,0,0,0,
75,76,1,0,0,0,76,77,1,0,0,0,77,79,5,2,0,0,78,80,3,2,1,0,79,78,1,0,0,0,80,
81,1,0,0,0,81,79,1,0,0,0,81,82,1,0,0,0,82,83,1,0,0,0,83,84,5,2,0,0,84,218,
1,0,0,0,85,86,5,1,0,0,86,87,5,9,0,0,87,88,3,16,8,0,88,92,3,18,9,0,89,91,
3,2,1,0,90,89,1,0,0,0,91,94,1,0,0,0,92,90,1,0,0,0,92,93,1,0,0,0,93,95,1,
0,0,0,94,92,1,0,0,0,95,96,5,2,0,0,96,218,1,0,0,0,97,98,5,1,0,0,98,99,5,10,
0,0,99,100,3,2,1,0,100,101,5,2,0,0,101,218,1,0,0,0,102,103,5,1,0,0,103,104,
5,11,0,0,104,105,3,20,10,0,105,107,3,22,11,0,106,108,3,24,12,0,107,106,1,
0,0,0,107,108,1,0,0,0,108,109,1,0,0,0,109,110,5,2,0,0,110,218,1,0,0,0,111,
112,5,1,0,0,112,113,5,12,0,0,113,117,5,1,0,0,114,116,3,12,6,0,115,114,1,
0,0,0,116,119,1,0,0,0,117,115,1,0,0,0,117,118,1,0,0,0,118,127,1,0,0,0,119,
117,1,0,0,0,120,124,5,3,0,0,121,123,3,14,7,0,122,121,1,0,0,0,123,126,1,0,
0,0,124,122,1,0,0,0,124,125,1,0,0,0,125,128,1,0,0,0,126,124,1,0,0,0,127,
120,1,0,0,0,127,128,1,0,0,0,128,129,1,0,0,0,129,131,5,2,0,0,130,132,3,2,
1,0,131,130,1,0,0,0,132,133,1,0,0,0,133,131,1,0,0,0,133,134,1,0,0,0,134,
135,1,0,0,0,135,136,5,2,0,0,136,218,1,0,0,0,137,138,5,1,0,0,138,142,5,13,
0,0,139,141,3,2,1,0,140,139,1,0,0,0,141,144,1,0,0,0,142,140,1,0,0,0,142,
143,1,0,0,0,143,145,1,0,0,0,144,142,1,0,0,0,145,218,5,2,0,0,146,147,5,1,
0,0,147,151,5,14,0,0,148,150,3,2,1,0,149,148,1,0,0,0,150,153,1,0,0,0,151,
149,1,0,0,0,151,152,1,0,0,0,152,154,1,0,0,0,153,151,1,0,0,0,154,218,5,2,
0,0,155,156,5,1,0,0,156,157,5,15,0,0,157,158,3,2,1,0,158,159,5,2,0,0,159,
218,1,0,0,0,160,161,5,1,0,0,161,162,5,16,0,0,162,166,3,26,13,0,163,165,3,
2,1,0,164,163,1,0,0,0,165,168,1,0,0,0,166,164,1,0,0,0,166,167,1,0,0,0,167,
169,1,0,0,0,168,166,1,0,0,0,169,170,5,2,0,0,170,218,1,0,0,0,171,172,5,1,
0,0,172,176,5,17,0,0,173,175,3,28,14,0,174,173,1,0,0,0,175,178,1,0,0,0,176,
174,1,0,0,0,176,177,1,0,0,0,177,179,1,0,0,0,178,176,1,0,0,0,179,218,5,2,
0,0,180,181,5,1,0,0,181,182,5,18,0,0,182,186,3,30,15,0,183,185,3,2,1,0,184,
183,1,0,0,0,185,188,1,0,0,0,186,184,1,0,0,0,186,187,1,0,0,0,187,189,1,0,
0,0,188,186,1,0,0,0,189,190,5,2,0,0,190,218,1,0,0,0,191,193,5,1,0,0,192,
194,3,32,16,0,193,192,1,0,0,0,194,195,1,0,0,0,195,193,1,0,0,0,195,196,1,
0,0,0,196,197,1,0,0,0,197,198,5,4,0,0,198,199,3,32,16,0,199,200,5,2,0,0,
200,218,1,0,0,0,201,205,5,1,0,0,202,204,3,32,16,0,203,202,1,0,0,0,204,207,
1,0,0,0,205,203,1,0,0,0,205,206,1,0,0,0,206,208,1,0,0,0,207,205,1,0,0,0,
208,218,5,2,0,0,209,218,5,19,0,0,210,218,5,20,0,0,211,218,5,21,0,0,212,218,
5,22,0,0,213,218,5,23,0,0,214,218,5,24,0,0,215,216,5,5,0,0,216,218,3,2,1,
0,217,40,1,0,0,0,217,49,1,0,0,0,217,58,1,0,0,0,217,85,1,0,0,0,217,97,1,0,
0,0,217,102,1,0,0,0,217,111,1,0,0,0,217,137,1,0,0,0,217,146,1,0,0,0,217,
155,1,0,0,0,217,160,1,0,0,0,217,171,1,0,0,0,217,180,1,0,0,0,217,191,1,0,
0,0,217,201,1,0,0,0,217,209,1,0,0,0,217,210,1,0,0,0,217,211,1,0,0,0,217,
212,1,0,0,0,217,213,1,0,0,0,217,214,1,0,0,0,217,215,1,0,0,0,218,3,1,0,0,
0,219,220,5,1,0,0,220,224,3,6,3,0,221,223,3,8,4,0,222,221,1,0,0,0,223,226,
1,0,0,0,224,222,1,0,0,0,224,225,1,0,0,0,225,227,1,0,0,0,226,224,1,0,0,0,
227,228,5,2,0,0,228,5,1,0,0,0,229,230,3,2,1,0,230,7,1,0,0,0,231,232,3,2,
1,0,232,9,1,0,0,0,233,234,5,24,0,0,234,11,1,0,0,0,235,236,5,24,0,0,236,13,
1,0,0,0,237,238,5,24,0,0,238,15,1,0,0,0,239,240,5,24,0,0,240,17,1,0,0,0,
241,242,3,2,1,0,242,19,1,0,0,0,243,244,3,2,1,0,244,21,1,0,0,0,245,246,3,
2,1,0,246,23,1,0,0,0,247,248,3,2,1,0,248,25,1,0,0,0,249,250,3,2,1,0,250,
27,1,0,0,0,251,252,5,24,0,0,252,253,3,2,1,0,253,29,1,0,0,0,254,255,3,2,1,
0,255,31,1,0,0,0,256,257,3,2,1,0,257,33,1,0,0,0,22,37,45,54,65,72,75,81,
92,107,117,124,127,133,142,151,166,176,186,195,205,217,224];


const atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

const decisionsToDFA = atn.decisionToState.map( (ds, index) => new antlr4.dfa.DFA(ds, index) );

const sharedContextCache = new antlr4.PredictionContextCache();

export default class VeLispParser extends antlr4.Parser {

    static grammarFileName = "VeLisp.g4";
    static literalNames = [ null, "'('", "')'", "' / '", "'.'", "'''" ];
    static symbolicNames = [ null, null, null, null, null, null, "AND", 
                             "COND", "DEFUN", "FOREACH", "FUNCTION", "IF", 
                             "LAMBDA", "OR", "PROGN", "QUOTE", "REPEAT", 
                             "SETQ", "WHILE", "NIL", "TRU", "INT", "REAL", 
                             "STR", "ID", "COMMENT", "LINE_COMMENT", "NEWLINE", 
                             "WHITESPACE" ];
    static ruleNames = [ "file", "expr", "condTestResult", "condTest", "condResult", 
                         "funName", "funParam", "funLocal", "foreachName", 
                         "foreachList", "ifTest", "ifThen", "ifElse", "repeatNum", 
                         "setqNameExpr", "whileTest", "listExpr" ];

    constructor(input) {
        super(input);
        this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
        this.ruleNames = VeLispParser.ruleNames;
        this.literalNames = VeLispParser.literalNames;
        this.symbolicNames = VeLispParser.symbolicNames;
    }

    get atn() {
        return atn;
    }



	file() {
	    let localctx = new FileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 0, VeLispParser.RULE_file);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 37;
	        this._errHandler.sync(this);
	        _la = this._input.LA(1);
	        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	            this.state = 34;
	            this.expr();
	            this.state = 39;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
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
	}



	expr() {
	    let localctx = new ExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 2, VeLispParser.RULE_expr);
	    var _la = 0; // Token type
	    try {
	        this.state = 217;
	        this._errHandler.sync(this);
	        var la_ = this._interp.adaptivePredict(this._input,20,this._ctx);
	        switch(la_) {
	        case 1:
	            localctx = new AndContext(this, localctx);
	            this.enterOuterAlt(localctx, 1);
	            this.state = 40;
	            this.match(VeLispParser.T__0);
	            this.state = 41;
	            this.match(VeLispParser.AND);
	            this.state = 45;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 42;
	                this.expr();
	                this.state = 47;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 48;
	            this.match(VeLispParser.T__1);
	            break;

	        case 2:
	            localctx = new CondContext(this, localctx);
	            this.enterOuterAlt(localctx, 2);
	            this.state = 49;
	            this.match(VeLispParser.T__0);
	            this.state = 50;
	            this.match(VeLispParser.COND);
	            this.state = 54;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.T__0) {
	                this.state = 51;
	                this.condTestResult();
	                this.state = 56;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 57;
	            this.match(VeLispParser.T__1);
	            break;

	        case 3:
	            localctx = new DefunContext(this, localctx);
	            this.enterOuterAlt(localctx, 3);
	            this.state = 58;
	            this.match(VeLispParser.T__0);
	            this.state = 59;
	            this.match(VeLispParser.DEFUN);
	            this.state = 60;
	            this.funName();
	            this.state = 61;
	            this.match(VeLispParser.T__0);
	            this.state = 65;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.ID) {
	                this.state = 62;
	                this.funParam();
	                this.state = 67;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 75;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeLispParser.T__2) {
	                this.state = 68;
	                this.match(VeLispParser.T__2);
	                this.state = 72;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	                while(_la===VeLispParser.ID) {
	                    this.state = 69;
	                    this.funLocal();
	                    this.state = 74;
	                    this._errHandler.sync(this);
	                    _la = this._input.LA(1);
	                }
	            }

	            this.state = 77;
	            this.match(VeLispParser.T__1);
	            this.state = 79; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 78;
	                this.expr();
	                this.state = 81; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 83;
	            this.match(VeLispParser.T__1);
	            break;

	        case 4:
	            localctx = new ForeachContext(this, localctx);
	            this.enterOuterAlt(localctx, 4);
	            this.state = 85;
	            this.match(VeLispParser.T__0);
	            this.state = 86;
	            this.match(VeLispParser.FOREACH);
	            this.state = 87;
	            this.foreachName();
	            this.state = 88;
	            this.foreachList();
	            this.state = 92;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 89;
	                this.expr();
	                this.state = 94;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 95;
	            this.match(VeLispParser.T__1);
	            break;

	        case 5:
	            localctx = new FunctionContext(this, localctx);
	            this.enterOuterAlt(localctx, 5);
	            this.state = 97;
	            this.match(VeLispParser.T__0);
	            this.state = 98;
	            this.match(VeLispParser.FUNCTION);
	            this.state = 99;
	            this.expr();
	            this.state = 100;
	            this.match(VeLispParser.T__1);
	            break;

	        case 6:
	            localctx = new IfContext(this, localctx);
	            this.enterOuterAlt(localctx, 6);
	            this.state = 102;
	            this.match(VeLispParser.T__0);
	            this.state = 103;
	            this.match(VeLispParser.IF);
	            this.state = 104;
	            this.ifTest();
	            this.state = 105;
	            this.ifThen();
	            this.state = 107;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 106;
	                this.ifElse();
	            }

	            this.state = 109;
	            this.match(VeLispParser.T__1);
	            break;

	        case 7:
	            localctx = new LambdaContext(this, localctx);
	            this.enterOuterAlt(localctx, 7);
	            this.state = 111;
	            this.match(VeLispParser.T__0);
	            this.state = 112;
	            this.match(VeLispParser.LAMBDA);
	            this.state = 113;
	            this.match(VeLispParser.T__0);
	            this.state = 117;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.ID) {
	                this.state = 114;
	                this.funParam();
	                this.state = 119;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 127;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeLispParser.T__2) {
	                this.state = 120;
	                this.match(VeLispParser.T__2);
	                this.state = 124;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	                while(_la===VeLispParser.ID) {
	                    this.state = 121;
	                    this.funLocal();
	                    this.state = 126;
	                    this._errHandler.sync(this);
	                    _la = this._input.LA(1);
	                }
	            }

	            this.state = 129;
	            this.match(VeLispParser.T__1);
	            this.state = 131; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 130;
	                this.expr();
	                this.state = 133; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 135;
	            this.match(VeLispParser.T__1);
	            break;

	        case 8:
	            localctx = new OrContext(this, localctx);
	            this.enterOuterAlt(localctx, 8);
	            this.state = 137;
	            this.match(VeLispParser.T__0);
	            this.state = 138;
	            this.match(VeLispParser.OR);
	            this.state = 142;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 139;
	                this.expr();
	                this.state = 144;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 145;
	            this.match(VeLispParser.T__1);
	            break;

	        case 9:
	            localctx = new PrognContext(this, localctx);
	            this.enterOuterAlt(localctx, 9);
	            this.state = 146;
	            this.match(VeLispParser.T__0);
	            this.state = 147;
	            this.match(VeLispParser.PROGN);
	            this.state = 151;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 148;
	                this.expr();
	                this.state = 153;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 154;
	            this.match(VeLispParser.T__1);
	            break;

	        case 10:
	            localctx = new QuoteContext(this, localctx);
	            this.enterOuterAlt(localctx, 10);
	            this.state = 155;
	            this.match(VeLispParser.T__0);
	            this.state = 156;
	            this.match(VeLispParser.QUOTE);
	            this.state = 157;
	            this.expr();
	            this.state = 158;
	            this.match(VeLispParser.T__1);
	            break;

	        case 11:
	            localctx = new RepeatContext(this, localctx);
	            this.enterOuterAlt(localctx, 11);
	            this.state = 160;
	            this.match(VeLispParser.T__0);
	            this.state = 161;
	            this.match(VeLispParser.REPEAT);
	            this.state = 162;
	            this.repeatNum();
	            this.state = 166;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 163;
	                this.expr();
	                this.state = 168;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 169;
	            this.match(VeLispParser.T__1);
	            break;

	        case 12:
	            localctx = new SetQContext(this, localctx);
	            this.enterOuterAlt(localctx, 12);
	            this.state = 171;
	            this.match(VeLispParser.T__0);
	            this.state = 172;
	            this.match(VeLispParser.SETQ);
	            this.state = 176;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.ID) {
	                this.state = 173;
	                this.setqNameExpr();
	                this.state = 178;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 179;
	            this.match(VeLispParser.T__1);
	            break;

	        case 13:
	            localctx = new WhileContext(this, localctx);
	            this.enterOuterAlt(localctx, 13);
	            this.state = 180;
	            this.match(VeLispParser.T__0);
	            this.state = 181;
	            this.match(VeLispParser.WHILE);
	            this.state = 182;
	            this.whileTest();
	            this.state = 186;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 183;
	                this.expr();
	                this.state = 188;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 189;
	            this.match(VeLispParser.T__1);
	            break;

	        case 14:
	            localctx = new DotListContext(this, localctx);
	            this.enterOuterAlt(localctx, 14);
	            this.state = 191;
	            this.match(VeLispParser.T__0);
	            this.state = 193; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 192;
	                this.listExpr();
	                this.state = 195; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 197;
	            this.match(VeLispParser.T__3);
	            this.state = 198;
	            this.listExpr();
	            this.state = 199;
	            this.match(VeLispParser.T__1);
	            break;

	        case 15:
	            localctx = new ListContext(this, localctx);
	            this.enterOuterAlt(localctx, 15);
	            this.state = 201;
	            this.match(VeLispParser.T__0);
	            this.state = 205;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 202;
	                this.listExpr();
	                this.state = 207;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 208;
	            this.match(VeLispParser.T__1);
	            break;

	        case 16:
	            localctx = new NilContext(this, localctx);
	            this.enterOuterAlt(localctx, 16);
	            this.state = 209;
	            this.match(VeLispParser.NIL);
	            break;

	        case 17:
	            localctx = new TruContext(this, localctx);
	            this.enterOuterAlt(localctx, 17);
	            this.state = 210;
	            this.match(VeLispParser.TRU);
	            break;

	        case 18:
	            localctx = new IntContext(this, localctx);
	            this.enterOuterAlt(localctx, 18);
	            this.state = 211;
	            this.match(VeLispParser.INT);
	            break;

	        case 19:
	            localctx = new RealContext(this, localctx);
	            this.enterOuterAlt(localctx, 19);
	            this.state = 212;
	            this.match(VeLispParser.REAL);
	            break;

	        case 20:
	            localctx = new StrContext(this, localctx);
	            this.enterOuterAlt(localctx, 20);
	            this.state = 213;
	            this.match(VeLispParser.STR);
	            break;

	        case 21:
	            localctx = new IdContext(this, localctx);
	            this.enterOuterAlt(localctx, 21);
	            this.state = 214;
	            this.match(VeLispParser.ID);
	            break;

	        case 22:
	            localctx = new QuoteContext(this, localctx);
	            this.enterOuterAlt(localctx, 22);
	            this.state = 215;
	            this.match(VeLispParser.T__4);
	            this.state = 216;
	            this.expr();
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
	}



	condTestResult() {
	    let localctx = new CondTestResultContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 4, VeLispParser.RULE_condTestResult);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 219;
	        this.match(VeLispParser.T__0);
	        this.state = 220;
	        this.condTest();
	        this.state = 224;
	        this._errHandler.sync(this);
	        _la = this._input.LA(1);
	        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	            this.state = 221;
	            this.condResult();
	            this.state = 226;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	        }
	        this.state = 227;
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
	}



	condTest() {
	    let localctx = new CondTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 6, VeLispParser.RULE_condTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 229;
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
	}



	condResult() {
	    let localctx = new CondResultContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 8, VeLispParser.RULE_condResult);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 231;
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
	}



	funName() {
	    let localctx = new FunNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 10, VeLispParser.RULE_funName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 233;
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
	}



	funParam() {
	    let localctx = new FunParamContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 12, VeLispParser.RULE_funParam);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 235;
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
	}



	funLocal() {
	    let localctx = new FunLocalContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 14, VeLispParser.RULE_funLocal);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 237;
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
	}



	foreachName() {
	    let localctx = new ForeachNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 16, VeLispParser.RULE_foreachName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 239;
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
	}



	foreachList() {
	    let localctx = new ForeachListContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 18, VeLispParser.RULE_foreachList);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 241;
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
	}



	ifTest() {
	    let localctx = new IfTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 20, VeLispParser.RULE_ifTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 243;
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
	}



	ifThen() {
	    let localctx = new IfThenContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 22, VeLispParser.RULE_ifThen);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 245;
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
	}



	ifElse() {
	    let localctx = new IfElseContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 24, VeLispParser.RULE_ifElse);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 247;
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
	}



	repeatNum() {
	    let localctx = new RepeatNumContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 26, VeLispParser.RULE_repeatNum);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 249;
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
	}



	setqNameExpr() {
	    let localctx = new SetqNameExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 28, VeLispParser.RULE_setqNameExpr);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 251;
	        this.match(VeLispParser.ID);
	        this.state = 252;
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
	}



	whileTest() {
	    let localctx = new WhileTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 30, VeLispParser.RULE_whileTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 254;
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
	}



	listExpr() {
	    let localctx = new ListExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 32, VeLispParser.RULE_listExpr);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 256;
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
	}


}

VeLispParser.EOF = antlr4.Token.EOF;
VeLispParser.T__0 = 1;
VeLispParser.T__1 = 2;
VeLispParser.T__2 = 3;
VeLispParser.T__3 = 4;
VeLispParser.T__4 = 5;
VeLispParser.AND = 6;
VeLispParser.COND = 7;
VeLispParser.DEFUN = 8;
VeLispParser.FOREACH = 9;
VeLispParser.FUNCTION = 10;
VeLispParser.IF = 11;
VeLispParser.LAMBDA = 12;
VeLispParser.OR = 13;
VeLispParser.PROGN = 14;
VeLispParser.QUOTE = 15;
VeLispParser.REPEAT = 16;
VeLispParser.SETQ = 17;
VeLispParser.WHILE = 18;
VeLispParser.NIL = 19;
VeLispParser.TRU = 20;
VeLispParser.INT = 21;
VeLispParser.REAL = 22;
VeLispParser.STR = 23;
VeLispParser.ID = 24;
VeLispParser.COMMENT = 25;
VeLispParser.LINE_COMMENT = 26;
VeLispParser.NEWLINE = 27;
VeLispParser.WHITESPACE = 28;

VeLispParser.RULE_file = 0;
VeLispParser.RULE_expr = 1;
VeLispParser.RULE_condTestResult = 2;
VeLispParser.RULE_condTest = 3;
VeLispParser.RULE_condResult = 4;
VeLispParser.RULE_funName = 5;
VeLispParser.RULE_funParam = 6;
VeLispParser.RULE_funLocal = 7;
VeLispParser.RULE_foreachName = 8;
VeLispParser.RULE_foreachList = 9;
VeLispParser.RULE_ifTest = 10;
VeLispParser.RULE_ifThen = 11;
VeLispParser.RULE_ifElse = 12;
VeLispParser.RULE_repeatNum = 13;
VeLispParser.RULE_setqNameExpr = 14;
VeLispParser.RULE_whileTest = 15;
VeLispParser.RULE_listExpr = 16;

class FileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_file;
    }

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFile(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_expr;
    }


	 
		copyFrom(ctx) {
			super.copyFrom(ctx);
		}

}


class OrContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	OR() {
	    return this.getToken(VeLispParser.OR, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitOr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.OrContext = OrContext;

class PrognContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	PROGN() {
	    return this.getToken(VeLispParser.PROGN, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitProgn(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.PrognContext = PrognContext;

class RealContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	REAL() {
	    return this.getToken(VeLispParser.REAL, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitReal(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.RealContext = RealContext;

class CondContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	COND() {
	    return this.getToken(VeLispParser.COND, 0);
	};

	condTestResult = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(CondTestResultContext);
	    } else {
	        return this.getTypedRuleContext(CondTestResultContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCond(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.CondContext = CondContext;

class WhileContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	WHILE() {
	    return this.getToken(VeLispParser.WHILE, 0);
	};

	whileTest() {
	    return this.getTypedRuleContext(WhileTestContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitWhile(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.WhileContext = WhileContext;

class ListContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	listExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ListExprContext);
	    } else {
	        return this.getTypedRuleContext(ListExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.ListContext = ListContext;

class DefunContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	DEFUN() {
	    return this.getToken(VeLispParser.DEFUN, 0);
	};

	funName() {
	    return this.getTypedRuleContext(FunNameContext,0);
	};

	funParam = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunParamContext);
	    } else {
	        return this.getTypedRuleContext(FunParamContext,i);
	    }
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	funLocal = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunLocalContext);
	    } else {
	        return this.getTypedRuleContext(FunLocalContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitDefun(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.DefunContext = DefunContext;

class IntContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	INT() {
	    return this.getToken(VeLispParser.INT, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitInt(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IntContext = IntContext;

class NilContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	NIL() {
	    return this.getToken(VeLispParser.NIL, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitNil(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.NilContext = NilContext;

class StrContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	STR() {
	    return this.getToken(VeLispParser.STR, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitStr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.StrContext = StrContext;

class ForeachContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	FOREACH() {
	    return this.getToken(VeLispParser.FOREACH, 0);
	};

	foreachName() {
	    return this.getTypedRuleContext(ForeachNameContext,0);
	};

	foreachList() {
	    return this.getTypedRuleContext(ForeachListContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeach(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.ForeachContext = ForeachContext;

class LambdaContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	LAMBDA() {
	    return this.getToken(VeLispParser.LAMBDA, 0);
	};

	funParam = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunParamContext);
	    } else {
	        return this.getTypedRuleContext(FunParamContext,i);
	    }
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	funLocal = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunLocalContext);
	    } else {
	        return this.getTypedRuleContext(FunLocalContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitLambda(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.LambdaContext = LambdaContext;

class QuoteContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	QUOTE() {
	    return this.getToken(VeLispParser.QUOTE, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitQuote(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.QuoteContext = QuoteContext;

class AndContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	AND() {
	    return this.getToken(VeLispParser.AND, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitAnd(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.AndContext = AndContext;

class TruContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	TRU() {
	    return this.getToken(VeLispParser.TRU, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitTru(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.TruContext = TruContext;

class FunctionContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	FUNCTION() {
	    return this.getToken(VeLispParser.FUNCTION, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunction(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.FunctionContext = FunctionContext;

class RepeatContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	REPEAT() {
	    return this.getToken(VeLispParser.REPEAT, 0);
	};

	repeatNum() {
	    return this.getTypedRuleContext(RepeatNumContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitRepeat(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.RepeatContext = RepeatContext;

class DotListContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	listExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ListExprContext);
	    } else {
	        return this.getTypedRuleContext(ListExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitDotList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.DotListContext = DotListContext;

class SetQContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	SETQ() {
	    return this.getToken(VeLispParser.SETQ, 0);
	};

	setqNameExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(SetqNameExprContext);
	    } else {
	        return this.getTypedRuleContext(SetqNameExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitSetQ(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.SetQContext = SetQContext;

class IdContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitId(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IdContext = IdContext;

class IfContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	IF() {
	    return this.getToken(VeLispParser.IF, 0);
	};

	ifTest() {
	    return this.getTypedRuleContext(IfTestContext,0);
	};

	ifThen() {
	    return this.getTypedRuleContext(IfThenContext,0);
	};

	ifElse() {
	    return this.getTypedRuleContext(IfElseContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIf(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IfContext = IfContext;

class CondTestResultContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condTestResult;
    }

	condTest() {
	    return this.getTypedRuleContext(CondTestContext,0);
	};

	condResult = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(CondResultContext);
	    } else {
	        return this.getTypedRuleContext(CondResultContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondTestResult(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class CondTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class CondResultContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condResult;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondResult(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funName;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunName(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunParamContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funParam;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunParam(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunLocalContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funLocal;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunLocal(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ForeachNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_foreachName;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeachName(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ForeachListContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_foreachList;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeachList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfThenContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifThen;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfThen(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfElseContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifElse;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfElse(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class RepeatNumContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_repeatNum;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitRepeatNum(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class SetqNameExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_setqNameExpr;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitSetqNameExpr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class WhileTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_whileTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitWhileTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ListExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_listExpr;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitListExpr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}




VeLispParser.FileContext = FileContext; 
VeLispParser.ExprContext = ExprContext; 
VeLispParser.CondTestResultContext = CondTestResultContext; 
VeLispParser.CondTestContext = CondTestContext; 
VeLispParser.CondResultContext = CondResultContext; 
VeLispParser.FunNameContext = FunNameContext; 
VeLispParser.FunParamContext = FunParamContext; 
VeLispParser.FunLocalContext = FunLocalContext; 
VeLispParser.ForeachNameContext = ForeachNameContext; 
VeLispParser.ForeachListContext = ForeachListContext; 
VeLispParser.IfTestContext = IfTestContext; 
VeLispParser.IfThenContext = IfThenContext; 
VeLispParser.IfElseContext = IfElseContext; 
VeLispParser.RepeatNumContext = RepeatNumContext; 
VeLispParser.SetqNameExprContext = SetqNameExprContext; 
VeLispParser.WhileTestContext = WhileTestContext; 
VeLispParser.ListExprContext = ListExprContext; 
