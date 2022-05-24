// Generated from grammar/VeDcl.g4 by ANTLR 4.10.1
// jshint ignore: start
import antlr4 from 'antlr4';
import VeDclListener from './VeDclListener.js';
const serializedATN = [4,1,40,136,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,
4,2,5,7,5,2,6,7,6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,
1,0,1,0,5,0,29,8,0,10,0,12,0,32,9,0,1,1,1,1,1,1,3,1,37,8,1,1,2,1,2,1,2,1,
2,1,2,5,2,44,8,2,10,2,12,2,47,9,2,1,2,1,2,3,2,51,8,2,1,2,1,2,1,2,1,2,1,2,
5,2,58,8,2,10,2,12,2,61,9,2,1,2,1,2,3,2,65,8,2,3,2,67,8,2,1,3,1,3,1,3,1,
3,5,3,73,8,3,10,3,12,3,76,9,3,1,3,1,3,3,3,80,8,3,1,3,1,3,1,3,1,3,5,3,86,
8,3,10,3,12,3,89,9,3,1,3,1,3,3,3,93,8,3,1,3,1,3,1,3,1,3,5,3,99,8,3,10,3,
12,3,102,9,3,1,3,1,3,3,3,106,8,3,1,3,1,3,1,3,3,3,111,8,3,1,4,1,4,1,5,1,5,
1,6,1,6,1,7,1,7,1,8,1,8,3,8,123,8,8,1,9,1,9,1,10,1,10,1,10,1,10,1,10,1,11,
1,11,1,12,1,12,1,12,0,0,13,0,2,4,6,8,10,12,14,16,18,20,22,24,0,3,1,0,6,16,
1,0,17,28,1,0,30,35,140,0,30,1,0,0,0,2,33,1,0,0,0,4,66,1,0,0,0,6,110,1,0,
0,0,8,112,1,0,0,0,10,114,1,0,0,0,12,116,1,0,0,0,14,118,1,0,0,0,16,122,1,
0,0,0,18,124,1,0,0,0,20,126,1,0,0,0,22,131,1,0,0,0,24,133,1,0,0,0,26,29,
3,2,1,0,27,29,3,4,2,0,28,26,1,0,0,0,28,27,1,0,0,0,29,32,1,0,0,0,30,28,1,
0,0,0,30,31,1,0,0,0,31,1,1,0,0,0,32,30,1,0,0,0,33,34,5,1,0,0,34,36,3,18,
9,0,35,37,5,2,0,0,36,35,1,0,0,0,36,37,1,0,0,0,37,3,1,0,0,0,38,39,5,36,0,
0,39,40,5,3,0,0,40,41,3,8,4,0,41,45,5,4,0,0,42,44,3,16,8,0,43,42,1,0,0,0,
44,47,1,0,0,0,45,43,1,0,0,0,45,46,1,0,0,0,46,48,1,0,0,0,47,45,1,0,0,0,48,
50,5,5,0,0,49,51,5,2,0,0,50,49,1,0,0,0,50,51,1,0,0,0,51,67,1,0,0,0,52,53,
5,36,0,0,53,54,5,3,0,0,54,55,3,10,5,0,55,59,5,4,0,0,56,58,3,20,10,0,57,56,
1,0,0,0,58,61,1,0,0,0,59,57,1,0,0,0,59,60,1,0,0,0,60,62,1,0,0,0,61,59,1,
0,0,0,62,64,5,5,0,0,63,65,5,2,0,0,64,63,1,0,0,0,64,65,1,0,0,0,65,67,1,0,
0,0,66,38,1,0,0,0,66,52,1,0,0,0,67,5,1,0,0,0,68,69,5,3,0,0,69,70,3,8,4,0,
70,74,5,4,0,0,71,73,3,16,8,0,72,71,1,0,0,0,73,76,1,0,0,0,74,72,1,0,0,0,74,
75,1,0,0,0,75,77,1,0,0,0,76,74,1,0,0,0,77,79,5,5,0,0,78,80,5,2,0,0,79,78,
1,0,0,0,79,80,1,0,0,0,80,111,1,0,0,0,81,82,5,3,0,0,82,83,3,10,5,0,83,87,
5,4,0,0,84,86,3,20,10,0,85,84,1,0,0,0,86,89,1,0,0,0,87,85,1,0,0,0,87,88,
1,0,0,0,88,90,1,0,0,0,89,87,1,0,0,0,90,92,5,5,0,0,91,93,5,2,0,0,92,91,1,
0,0,0,92,93,1,0,0,0,93,111,1,0,0,0,94,95,5,3,0,0,95,96,3,12,6,0,96,100,5,
4,0,0,97,99,3,20,10,0,98,97,1,0,0,0,99,102,1,0,0,0,100,98,1,0,0,0,100,101,
1,0,0,0,101,103,1,0,0,0,102,100,1,0,0,0,103,105,5,5,0,0,104,106,5,2,0,0,
105,104,1,0,0,0,105,106,1,0,0,0,106,111,1,0,0,0,107,108,3,14,7,0,108,109,
5,2,0,0,109,111,1,0,0,0,110,68,1,0,0,0,110,81,1,0,0,0,110,94,1,0,0,0,110,
107,1,0,0,0,111,7,1,0,0,0,112,113,7,0,0,0,113,9,1,0,0,0,114,115,7,1,0,0,
115,11,1,0,0,0,116,117,5,36,0,0,117,13,1,0,0,0,118,119,5,36,0,0,119,15,1,
0,0,0,120,123,3,20,10,0,121,123,3,6,3,0,122,120,1,0,0,0,122,121,1,0,0,0,
123,17,1,0,0,0,124,125,5,33,0,0,125,19,1,0,0,0,126,127,3,22,11,0,127,128,
5,29,0,0,128,129,3,24,12,0,129,130,5,2,0,0,130,21,1,0,0,0,131,132,5,36,0,
0,132,23,1,0,0,0,133,134,7,2,0,0,134,25,1,0,0,0,16,28,30,36,45,50,59,64,
66,74,79,87,92,100,105,110,122];


const atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

const decisionsToDFA = atn.decisionToState.map( (ds, index) => new antlr4.dfa.DFA(ds, index) );

const sharedContextCache = new antlr4.PredictionContextCache();

export default class VeDclParser extends antlr4.Parser {

    static grammarFileName = "VeDcl.g4";
    static literalNames = [ null, "'@include'", "';'", "':'", "'{'", "'}'", 
                            "'dialog'", "'row'", "'column'", "'boxed_row'", 
                            "'boxed_column'", "'concatenation'", "'paragraph'", 
                            "'radio_row'", "'radio_column'", "'boxed_radio_row'", 
                            "'boxed_radio_column'", "'button'", "'edit_box'", 
                            "'image'", "'image_button'", "'list_box'", "'popup_list'", 
                            "'radio_button'", "'slider'", "'spacer'", "'text'", 
                            "'text_part'", "'toggle'", "'='" ];
    static symbolicNames = [ null, null, null, null, null, null, null, null, 
                             null, null, null, null, null, null, null, null, 
                             null, null, null, null, null, null, null, null, 
                             null, null, null, null, null, null, "BOOL", 
                             "INT", "REAL", "STR", "ALIGN", "LAYOUT", "ID", 
                             "COMMENT", "LINE_COMMENT", "NEWLINE", "WHITESPACE" ];
    static ruleNames = [ "file", "includeFile", "defineTile", "innerTile", 
                         "clusterTile", "simpleTile", "deriveTile", "aliasTile", 
                         "entry", "fileName", "attribute", "attributeName", 
                         "attributeValue" ];

    constructor(input) {
        super(input);
        this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
        this.ruleNames = VeDclParser.ruleNames;
        this.literalNames = VeDclParser.literalNames;
        this.symbolicNames = VeDclParser.symbolicNames;
    }

    get atn() {
        return atn;
    }



	file() {
	    let localctx = new FileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 0, VeDclParser.RULE_file);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 30;
	        this._errHandler.sync(this);
	        _la = this._input.LA(1);
	        while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
	            this.state = 28;
	            this._errHandler.sync(this);
	            switch(this._input.LA(1)) {
	            case VeDclParser.T__0:
	                this.state = 26;
	                this.includeFile();
	                break;
	            case VeDclParser.ID:
	                this.state = 27;
	                this.defineTile();
	                break;
	            default:
	                throw new antlr4.error.NoViableAltException(this);
	            }
	            this.state = 32;
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



	includeFile() {
	    let localctx = new IncludeFileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 2, VeDclParser.RULE_includeFile);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 33;
	        this.match(VeDclParser.T__0);
	        this.state = 34;
	        this.fileName();
	        this.state = 36;
	        this._errHandler.sync(this);
	        _la = this._input.LA(1);
	        if(_la===VeDclParser.T__1) {
	            this.state = 35;
	            this.match(VeDclParser.T__1);
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



	defineTile() {
	    let localctx = new DefineTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 4, VeDclParser.RULE_defineTile);
	    var _la = 0; // Token type
	    try {
	        this.state = 66;
	        this._errHandler.sync(this);
	        var la_ = this._interp.adaptivePredict(this._input,7,this._ctx);
	        switch(la_) {
	        case 1:
	            localctx = new DefineClusterTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 1);
	            this.state = 38;
	            this.match(VeDclParser.ID);
	            this.state = 39;
	            this.match(VeDclParser.T__2);
	            this.state = 40;
	            this.clusterTile();
	            this.state = 41;
	            this.match(VeDclParser.T__3);
	            this.state = 45;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeDclParser.T__2 || _la===VeDclParser.ID) {
	                this.state = 42;
	                this.entry();
	                this.state = 47;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 48;
	            this.match(VeDclParser.T__4);
	            this.state = 50;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeDclParser.T__1) {
	                this.state = 49;
	                this.match(VeDclParser.T__1);
	            }

	            break;

	        case 2:
	            localctx = new DefineSimpleTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 2);
	            this.state = 52;
	            this.match(VeDclParser.ID);
	            this.state = 53;
	            this.match(VeDclParser.T__2);
	            this.state = 54;
	            this.simpleTile();
	            this.state = 55;
	            this.match(VeDclParser.T__3);
	            this.state = 59;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeDclParser.ID) {
	                this.state = 56;
	                this.attribute();
	                this.state = 61;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 62;
	            this.match(VeDclParser.T__4);
	            this.state = 64;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeDclParser.T__1) {
	                this.state = 63;
	                this.match(VeDclParser.T__1);
	            }

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



	innerTile() {
	    let localctx = new InnerTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 6, VeDclParser.RULE_innerTile);
	    var _la = 0; // Token type
	    try {
	        this.state = 110;
	        this._errHandler.sync(this);
	        var la_ = this._interp.adaptivePredict(this._input,14,this._ctx);
	        switch(la_) {
	        case 1:
	            localctx = new InnerClusterTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 1);
	            this.state = 68;
	            this.match(VeDclParser.T__2);
	            this.state = 69;
	            this.clusterTile();
	            this.state = 70;
	            this.match(VeDclParser.T__3);
	            this.state = 74;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeDclParser.T__2 || _la===VeDclParser.ID) {
	                this.state = 71;
	                this.entry();
	                this.state = 76;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 77;
	            this.match(VeDclParser.T__4);
	            this.state = 79;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeDclParser.T__1) {
	                this.state = 78;
	                this.match(VeDclParser.T__1);
	            }

	            break;

	        case 2:
	            localctx = new InnerSimpleTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 2);
	            this.state = 81;
	            this.match(VeDclParser.T__2);
	            this.state = 82;
	            this.simpleTile();
	            this.state = 83;
	            this.match(VeDclParser.T__3);
	            this.state = 87;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeDclParser.ID) {
	                this.state = 84;
	                this.attribute();
	                this.state = 89;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 90;
	            this.match(VeDclParser.T__4);
	            this.state = 92;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeDclParser.T__1) {
	                this.state = 91;
	                this.match(VeDclParser.T__1);
	            }

	            break;

	        case 3:
	            localctx = new InnerDeriveTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 3);
	            this.state = 94;
	            this.match(VeDclParser.T__2);
	            this.state = 95;
	            this.deriveTile();
	            this.state = 96;
	            this.match(VeDclParser.T__3);
	            this.state = 100;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeDclParser.ID) {
	                this.state = 97;
	                this.attribute();
	                this.state = 102;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 103;
	            this.match(VeDclParser.T__4);
	            this.state = 105;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeDclParser.T__1) {
	                this.state = 104;
	                this.match(VeDclParser.T__1);
	            }

	            break;

	        case 4:
	            localctx = new InnerAliasTileContext(this, localctx);
	            this.enterOuterAlt(localctx, 4);
	            this.state = 107;
	            this.aliasTile();
	            this.state = 108;
	            this.match(VeDclParser.T__1);
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



	clusterTile() {
	    let localctx = new ClusterTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 8, VeDclParser.RULE_clusterTile);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 112;
	        _la = this._input.LA(1);
	        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__5) | (1 << VeDclParser.T__6) | (1 << VeDclParser.T__7) | (1 << VeDclParser.T__8) | (1 << VeDclParser.T__9) | (1 << VeDclParser.T__10) | (1 << VeDclParser.T__11) | (1 << VeDclParser.T__12) | (1 << VeDclParser.T__13) | (1 << VeDclParser.T__14) | (1 << VeDclParser.T__15))) !== 0))) {
	        this._errHandler.recoverInline(this);
	        }
	        else {
	        	this._errHandler.reportMatch(this);
	            this.consume();
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



	simpleTile() {
	    let localctx = new SimpleTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 10, VeDclParser.RULE_simpleTile);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 114;
	        _la = this._input.LA(1);
	        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__16) | (1 << VeDclParser.T__17) | (1 << VeDclParser.T__18) | (1 << VeDclParser.T__19) | (1 << VeDclParser.T__20) | (1 << VeDclParser.T__21) | (1 << VeDclParser.T__22) | (1 << VeDclParser.T__23) | (1 << VeDclParser.T__24) | (1 << VeDclParser.T__25) | (1 << VeDclParser.T__26) | (1 << VeDclParser.T__27))) !== 0))) {
	        this._errHandler.recoverInline(this);
	        }
	        else {
	        	this._errHandler.reportMatch(this);
	            this.consume();
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



	deriveTile() {
	    let localctx = new DeriveTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 12, VeDclParser.RULE_deriveTile);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 116;
	        this.match(VeDclParser.ID);
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



	aliasTile() {
	    let localctx = new AliasTileContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 14, VeDclParser.RULE_aliasTile);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 118;
	        this.match(VeDclParser.ID);
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



	entry() {
	    let localctx = new EntryContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 16, VeDclParser.RULE_entry);
	    try {
	        this.state = 122;
	        this._errHandler.sync(this);
	        var la_ = this._interp.adaptivePredict(this._input,15,this._ctx);
	        switch(la_) {
	        case 1:
	            this.enterOuterAlt(localctx, 1);
	            this.state = 120;
	            this.attribute();
	            break;

	        case 2:
	            this.enterOuterAlt(localctx, 2);
	            this.state = 121;
	            this.innerTile();
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



	fileName() {
	    let localctx = new FileNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 18, VeDclParser.RULE_fileName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 124;
	        this.match(VeDclParser.STR);
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



	attribute() {
	    let localctx = new AttributeContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 20, VeDclParser.RULE_attribute);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 126;
	        this.attributeName();
	        this.state = 127;
	        this.match(VeDclParser.T__28);
	        this.state = 128;
	        this.attributeValue();
	        this.state = 129;
	        this.match(VeDclParser.T__1);
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



	attributeName() {
	    let localctx = new AttributeNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 22, VeDclParser.RULE_attributeName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 131;
	        this.match(VeDclParser.ID);
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



	attributeValue() {
	    let localctx = new AttributeValueContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 24, VeDclParser.RULE_attributeValue);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 133;
	        _la = this._input.LA(1);
	        if(!(((((_la - 30)) & ~0x1f) == 0 && ((1 << (_la - 30)) & ((1 << (VeDclParser.BOOL - 30)) | (1 << (VeDclParser.INT - 30)) | (1 << (VeDclParser.REAL - 30)) | (1 << (VeDclParser.STR - 30)) | (1 << (VeDclParser.ALIGN - 30)) | (1 << (VeDclParser.LAYOUT - 30)))) !== 0))) {
	        this._errHandler.recoverInline(this);
	        }
	        else {
	        	this._errHandler.reportMatch(this);
	            this.consume();
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


}

VeDclParser.EOF = antlr4.Token.EOF;
VeDclParser.T__0 = 1;
VeDclParser.T__1 = 2;
VeDclParser.T__2 = 3;
VeDclParser.T__3 = 4;
VeDclParser.T__4 = 5;
VeDclParser.T__5 = 6;
VeDclParser.T__6 = 7;
VeDclParser.T__7 = 8;
VeDclParser.T__8 = 9;
VeDclParser.T__9 = 10;
VeDclParser.T__10 = 11;
VeDclParser.T__11 = 12;
VeDclParser.T__12 = 13;
VeDclParser.T__13 = 14;
VeDclParser.T__14 = 15;
VeDclParser.T__15 = 16;
VeDclParser.T__16 = 17;
VeDclParser.T__17 = 18;
VeDclParser.T__18 = 19;
VeDclParser.T__19 = 20;
VeDclParser.T__20 = 21;
VeDclParser.T__21 = 22;
VeDclParser.T__22 = 23;
VeDclParser.T__23 = 24;
VeDclParser.T__24 = 25;
VeDclParser.T__25 = 26;
VeDclParser.T__26 = 27;
VeDclParser.T__27 = 28;
VeDclParser.T__28 = 29;
VeDclParser.BOOL = 30;
VeDclParser.INT = 31;
VeDclParser.REAL = 32;
VeDclParser.STR = 33;
VeDclParser.ALIGN = 34;
VeDclParser.LAYOUT = 35;
VeDclParser.ID = 36;
VeDclParser.COMMENT = 37;
VeDclParser.LINE_COMMENT = 38;
VeDclParser.NEWLINE = 39;
VeDclParser.WHITESPACE = 40;

VeDclParser.RULE_file = 0;
VeDclParser.RULE_includeFile = 1;
VeDclParser.RULE_defineTile = 2;
VeDclParser.RULE_innerTile = 3;
VeDclParser.RULE_clusterTile = 4;
VeDclParser.RULE_simpleTile = 5;
VeDclParser.RULE_deriveTile = 6;
VeDclParser.RULE_aliasTile = 7;
VeDclParser.RULE_entry = 8;
VeDclParser.RULE_fileName = 9;
VeDclParser.RULE_attribute = 10;
VeDclParser.RULE_attributeName = 11;
VeDclParser.RULE_attributeValue = 12;

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
        this.ruleIndex = VeDclParser.RULE_file;
    }

	includeFile = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(IncludeFileContext);
	    } else {
	        return this.getTypedRuleContext(IncludeFileContext,i);
	    }
	};

	defineTile = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(DefineTileContext);
	    } else {
	        return this.getTypedRuleContext(DefineTileContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterFile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitFile(this);
		}
	}


}



class IncludeFileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_includeFile;
    }

	fileName() {
	    return this.getTypedRuleContext(FileNameContext,0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterIncludeFile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitIncludeFile(this);
		}
	}


}



class DefineTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_defineTile;
    }


	 
		copyFrom(ctx) {
			super.copyFrom(ctx);
		}

}


class DefineClusterTileContext extends DefineTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	ID() {
	    return this.getToken(VeDclParser.ID, 0);
	};

	clusterTile() {
	    return this.getTypedRuleContext(ClusterTileContext,0);
	};

	entry = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(EntryContext);
	    } else {
	        return this.getTypedRuleContext(EntryContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterDefineClusterTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitDefineClusterTile(this);
		}
	}


}

VeDclParser.DefineClusterTileContext = DefineClusterTileContext;

class DefineSimpleTileContext extends DefineTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	ID() {
	    return this.getToken(VeDclParser.ID, 0);
	};

	simpleTile() {
	    return this.getTypedRuleContext(SimpleTileContext,0);
	};

	attribute = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(AttributeContext);
	    } else {
	        return this.getTypedRuleContext(AttributeContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterDefineSimpleTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitDefineSimpleTile(this);
		}
	}


}

VeDclParser.DefineSimpleTileContext = DefineSimpleTileContext;

class InnerTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_innerTile;
    }


	 
		copyFrom(ctx) {
			super.copyFrom(ctx);
		}

}


class InnerAliasTileContext extends InnerTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	aliasTile() {
	    return this.getTypedRuleContext(AliasTileContext,0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterInnerAliasTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitInnerAliasTile(this);
		}
	}


}

VeDclParser.InnerAliasTileContext = InnerAliasTileContext;

class InnerSimpleTileContext extends InnerTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	simpleTile() {
	    return this.getTypedRuleContext(SimpleTileContext,0);
	};

	attribute = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(AttributeContext);
	    } else {
	        return this.getTypedRuleContext(AttributeContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterInnerSimpleTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitInnerSimpleTile(this);
		}
	}


}

VeDclParser.InnerSimpleTileContext = InnerSimpleTileContext;

class InnerDeriveTileContext extends InnerTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	deriveTile() {
	    return this.getTypedRuleContext(DeriveTileContext,0);
	};

	attribute = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(AttributeContext);
	    } else {
	        return this.getTypedRuleContext(AttributeContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterInnerDeriveTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitInnerDeriveTile(this);
		}
	}


}

VeDclParser.InnerDeriveTileContext = InnerDeriveTileContext;

class InnerClusterTileContext extends InnerTileContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	clusterTile() {
	    return this.getTypedRuleContext(ClusterTileContext,0);
	};

	entry = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(EntryContext);
	    } else {
	        return this.getTypedRuleContext(EntryContext,i);
	    }
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterInnerClusterTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitInnerClusterTile(this);
		}
	}


}

VeDclParser.InnerClusterTileContext = InnerClusterTileContext;

class ClusterTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_clusterTile;
    }


	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterClusterTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitClusterTile(this);
		}
	}


}



class SimpleTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_simpleTile;
    }


	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterSimpleTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitSimpleTile(this);
		}
	}


}



class DeriveTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_deriveTile;
    }

	ID() {
	    return this.getToken(VeDclParser.ID, 0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterDeriveTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitDeriveTile(this);
		}
	}


}



class AliasTileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_aliasTile;
    }

	ID() {
	    return this.getToken(VeDclParser.ID, 0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterAliasTile(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitAliasTile(this);
		}
	}


}



class EntryContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_entry;
    }

	attribute() {
	    return this.getTypedRuleContext(AttributeContext,0);
	};

	innerTile() {
	    return this.getTypedRuleContext(InnerTileContext,0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterEntry(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitEntry(this);
		}
	}


}



class FileNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_fileName;
    }

	STR() {
	    return this.getToken(VeDclParser.STR, 0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterFileName(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitFileName(this);
		}
	}


}



class AttributeContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_attribute;
    }

	attributeName() {
	    return this.getTypedRuleContext(AttributeNameContext,0);
	};

	attributeValue() {
	    return this.getTypedRuleContext(AttributeValueContext,0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterAttribute(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitAttribute(this);
		}
	}


}



class AttributeNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_attributeName;
    }

	ID() {
	    return this.getToken(VeDclParser.ID, 0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterAttributeName(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitAttributeName(this);
		}
	}


}



class AttributeValueContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeDclParser.RULE_attributeValue;
    }

	BOOL() {
	    return this.getToken(VeDclParser.BOOL, 0);
	};

	INT() {
	    return this.getToken(VeDclParser.INT, 0);
	};

	REAL() {
	    return this.getToken(VeDclParser.REAL, 0);
	};

	STR() {
	    return this.getToken(VeDclParser.STR, 0);
	};

	ALIGN() {
	    return this.getToken(VeDclParser.ALIGN, 0);
	};

	LAYOUT() {
	    return this.getToken(VeDclParser.LAYOUT, 0);
	};

	enterRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.enterAttributeValue(this);
		}
	}

	exitRule(listener) {
	    if(listener instanceof VeDclListener ) {
	        listener.exitAttributeValue(this);
		}
	}


}




VeDclParser.FileContext = FileContext; 
VeDclParser.IncludeFileContext = IncludeFileContext; 
VeDclParser.DefineTileContext = DefineTileContext; 
VeDclParser.InnerTileContext = InnerTileContext; 
VeDclParser.ClusterTileContext = ClusterTileContext; 
VeDclParser.SimpleTileContext = SimpleTileContext; 
VeDclParser.DeriveTileContext = DeriveTileContext; 
VeDclParser.AliasTileContext = AliasTileContext; 
VeDclParser.EntryContext = EntryContext; 
VeDclParser.FileNameContext = FileNameContext; 
VeDclParser.AttributeContext = AttributeContext; 
VeDclParser.AttributeNameContext = AttributeNameContext; 
VeDclParser.AttributeValueContext = AttributeValueContext; 
