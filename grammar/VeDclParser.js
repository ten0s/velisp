// Generated from grammar/VeDcl.g4 by ANTLR 4.9.3
// jshint ignore: start
import antlr4 from 'antlr4';
import VeDclListener from './VeDclListener.js';

const serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786",
    "\u5964\u0003*\u008a\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0004\u000e\t\u000e\u0003\u0002\u0003\u0002\u0007\u0002",
    "\u001f\n\u0002\f\u0002\u000e\u0002\"\u000b\u0002\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0005\u0003\'\n\u0003\u0003\u0004\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0004\u0007\u0004.\n\u0004\f\u0004\u000e\u00041\u000b",
    "\u0004\u0003\u0004\u0003\u0004\u0005\u00045\n\u0004\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0007\u0004<\n\u0004\f\u0004",
    "\u000e\u0004?\u000b\u0004\u0003\u0004\u0003\u0004\u0005\u0004C\n\u0004",
    "\u0005\u0004E\n\u0004\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005",
    "\u0007\u0005K\n\u0005\f\u0005\u000e\u0005N\u000b\u0005\u0003\u0005\u0003",
    "\u0005\u0005\u0005R\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003",
    "\u0005\u0007\u0005X\n\u0005\f\u0005\u000e\u0005[\u000b\u0005\u0003\u0005",
    "\u0003\u0005\u0005\u0005_\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005",
    "\u0003\u0005\u0007\u0005e\n\u0005\f\u0005\u000e\u0005h\u000b\u0005\u0003",
    "\u0005\u0003\u0005\u0005\u0005l\n\u0005\u0003\u0005\u0003\u0005\u0003",
    "\u0005\u0005\u0005q\n\u0005\u0003\u0006\u0003\u0006\u0003\u0007\u0003",
    "\u0007\u0003\b\u0003\b\u0003\t\u0003\t\u0003\n\u0003\n\u0005\n}\n\n",
    "\u0003\u000b\u0003\u000b\u0003\f\u0003\f\u0003\f\u0003\f\u0003\f\u0003",
    "\r\u0003\r\u0003\u000e\u0003\u000e\u0003\u000e\u0002\u0002\u000f\u0002",
    "\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u0002\u0005",
    "\u0003\u0002\b\u0012\u0003\u0002\u0013\u001e\u0003\u0002 %\u0002\u008e",
    "\u0002 \u0003\u0002\u0002\u0002\u0004#\u0003\u0002\u0002\u0002\u0006",
    "D\u0003\u0002\u0002\u0002\bp\u0003\u0002\u0002\u0002\nr\u0003\u0002",
    "\u0002\u0002\ft\u0003\u0002\u0002\u0002\u000ev\u0003\u0002\u0002\u0002",
    "\u0010x\u0003\u0002\u0002\u0002\u0012|\u0003\u0002\u0002\u0002\u0014",
    "~\u0003\u0002\u0002\u0002\u0016\u0080\u0003\u0002\u0002\u0002\u0018",
    "\u0085\u0003\u0002\u0002\u0002\u001a\u0087\u0003\u0002\u0002\u0002\u001c",
    "\u001f\u0005\u0004\u0003\u0002\u001d\u001f\u0005\u0006\u0004\u0002\u001e",
    "\u001c\u0003\u0002\u0002\u0002\u001e\u001d\u0003\u0002\u0002\u0002\u001f",
    "\"\u0003\u0002\u0002\u0002 \u001e\u0003\u0002\u0002\u0002 !\u0003\u0002",
    "\u0002\u0002!\u0003\u0003\u0002\u0002\u0002\" \u0003\u0002\u0002\u0002",
    "#$\u0007\u0003\u0002\u0002$&\u0005\u0014\u000b\u0002%\'\u0007\u0004",
    "\u0002\u0002&%\u0003\u0002\u0002\u0002&\'\u0003\u0002\u0002\u0002\'",
    "\u0005\u0003\u0002\u0002\u0002()\u0007&\u0002\u0002)*\u0007\u0005\u0002",
    "\u0002*+\u0005\n\u0006\u0002+/\u0007\u0006\u0002\u0002,.\u0005\u0012",
    "\n\u0002-,\u0003\u0002\u0002\u0002.1\u0003\u0002\u0002\u0002/-\u0003",
    "\u0002\u0002\u0002/0\u0003\u0002\u0002\u000202\u0003\u0002\u0002\u0002",
    "1/\u0003\u0002\u0002\u000224\u0007\u0007\u0002\u000235\u0007\u0004\u0002",
    "\u000243\u0003\u0002\u0002\u000245\u0003\u0002\u0002\u00025E\u0003\u0002",
    "\u0002\u000267\u0007&\u0002\u000278\u0007\u0005\u0002\u000289\u0005",
    "\f\u0007\u00029=\u0007\u0006\u0002\u0002:<\u0005\u0016\f\u0002;:\u0003",
    "\u0002\u0002\u0002<?\u0003\u0002\u0002\u0002=;\u0003\u0002\u0002\u0002",
    "=>\u0003\u0002\u0002\u0002>@\u0003\u0002\u0002\u0002?=\u0003\u0002\u0002",
    "\u0002@B\u0007\u0007\u0002\u0002AC\u0007\u0004\u0002\u0002BA\u0003\u0002",
    "\u0002\u0002BC\u0003\u0002\u0002\u0002CE\u0003\u0002\u0002\u0002D(\u0003",
    "\u0002\u0002\u0002D6\u0003\u0002\u0002\u0002E\u0007\u0003\u0002\u0002",
    "\u0002FG\u0007\u0005\u0002\u0002GH\u0005\n\u0006\u0002HL\u0007\u0006",
    "\u0002\u0002IK\u0005\u0012\n\u0002JI\u0003\u0002\u0002\u0002KN\u0003",
    "\u0002\u0002\u0002LJ\u0003\u0002\u0002\u0002LM\u0003\u0002\u0002\u0002",
    "MO\u0003\u0002\u0002\u0002NL\u0003\u0002\u0002\u0002OQ\u0007\u0007\u0002",
    "\u0002PR\u0007\u0004\u0002\u0002QP\u0003\u0002\u0002\u0002QR\u0003\u0002",
    "\u0002\u0002Rq\u0003\u0002\u0002\u0002ST\u0007\u0005\u0002\u0002TU\u0005",
    "\f\u0007\u0002UY\u0007\u0006\u0002\u0002VX\u0005\u0016\f\u0002WV\u0003",
    "\u0002\u0002\u0002X[\u0003\u0002\u0002\u0002YW\u0003\u0002\u0002\u0002",
    "YZ\u0003\u0002\u0002\u0002Z\\\u0003\u0002\u0002\u0002[Y\u0003\u0002",
    "\u0002\u0002\\^\u0007\u0007\u0002\u0002]_\u0007\u0004\u0002\u0002^]",
    "\u0003\u0002\u0002\u0002^_\u0003\u0002\u0002\u0002_q\u0003\u0002\u0002",
    "\u0002`a\u0007\u0005\u0002\u0002ab\u0005\u000e\b\u0002bf\u0007\u0006",
    "\u0002\u0002ce\u0005\u0016\f\u0002dc\u0003\u0002\u0002\u0002eh\u0003",
    "\u0002\u0002\u0002fd\u0003\u0002\u0002\u0002fg\u0003\u0002\u0002\u0002",
    "gi\u0003\u0002\u0002\u0002hf\u0003\u0002\u0002\u0002ik\u0007\u0007\u0002",
    "\u0002jl\u0007\u0004\u0002\u0002kj\u0003\u0002\u0002\u0002kl\u0003\u0002",
    "\u0002\u0002lq\u0003\u0002\u0002\u0002mn\u0005\u0010\t\u0002no\u0007",
    "\u0004\u0002\u0002oq\u0003\u0002\u0002\u0002pF\u0003\u0002\u0002\u0002",
    "pS\u0003\u0002\u0002\u0002p`\u0003\u0002\u0002\u0002pm\u0003\u0002\u0002",
    "\u0002q\t\u0003\u0002\u0002\u0002rs\t\u0002\u0002\u0002s\u000b\u0003",
    "\u0002\u0002\u0002tu\t\u0003\u0002\u0002u\r\u0003\u0002\u0002\u0002",
    "vw\u0007&\u0002\u0002w\u000f\u0003\u0002\u0002\u0002xy\u0007&\u0002",
    "\u0002y\u0011\u0003\u0002\u0002\u0002z}\u0005\u0016\f\u0002{}\u0005",
    "\b\u0005\u0002|z\u0003\u0002\u0002\u0002|{\u0003\u0002\u0002\u0002}",
    "\u0013\u0003\u0002\u0002\u0002~\u007f\u0007#\u0002\u0002\u007f\u0015",
    "\u0003\u0002\u0002\u0002\u0080\u0081\u0005\u0018\r\u0002\u0081\u0082",
    "\u0007\u001f\u0002\u0002\u0082\u0083\u0005\u001a\u000e\u0002\u0083\u0084",
    "\u0007\u0004\u0002\u0002\u0084\u0017\u0003\u0002\u0002\u0002\u0085\u0086",
    "\u0007&\u0002\u0002\u0086\u0019\u0003\u0002\u0002\u0002\u0087\u0088",
    "\t\u0004\u0002\u0002\u0088\u001b\u0003\u0002\u0002\u0002\u0012\u001e",
    " &/4=BDLQY^fkp|"].join("");


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
