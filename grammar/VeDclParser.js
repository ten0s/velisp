// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeDclListener = require('./VeDclListener').VeDclListener;
var grammarFileName = "VeDcl.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u0018\u0083\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0003\u0002\u0003\u0002\u0007\u0002\u001d\n\u0002\f\u0002",
    "\u000e\u0002 \u000b\u0002\u0003\u0003\u0003\u0003\u0003\u0003\u0005",
    "\u0003%\n\u0003\u0003\u0004\u0003\u0004\u0003\u0004\u0003\u0004\u0003",
    "\u0004\u0007\u0004,\n\u0004\f\u0004\u000e\u0004/\u000b\u0004\u0003\u0004",
    "\u0003\u0004\u0005\u00043\n\u0004\u0003\u0004\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0004\u0007\u0004:\n\u0004\f\u0004\u000e\u0004=\u000b",
    "\u0004\u0003\u0004\u0003\u0004\u0005\u0004A\n\u0004\u0005\u0004C\n\u0004",
    "\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0007\u0005I\n\u0005",
    "\f\u0005\u000e\u0005L\u000b\u0005\u0003\u0005\u0003\u0005\u0005\u0005",
    "P\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0007\u0005",
    "V\n\u0005\f\u0005\u000e\u0005Y\u000b\u0005\u0003\u0005\u0003\u0005\u0005",
    "\u0005]\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0007",
    "\u0005c\n\u0005\f\u0005\u000e\u0005f\u000b\u0005\u0003\u0005\u0003\u0005",
    "\u0005\u0005j\n\u0005\u0005\u0005l\n\u0005\u0003\u0006\u0003\u0006\u0003",
    "\u0007\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t\u0005\tv\n\t\u0003",
    "\n\u0003\n\u0003\u000b\u0003\u000b\u0003\u000b\u0003\u000b\u0003\u000b",
    "\u0003\f\u0003\f\u0003\r\u0003\r\u0003\r\u0002\u0002\u000e\u0002\u0004",
    "\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u0002\u0005\u0003\u0002",
    "\b\n\u0003\u0002\u000b\r\u0003\u0002\u000f\u0013\u0002\u0087\u0002\u001e",
    "\u0003\u0002\u0002\u0002\u0004!\u0003\u0002\u0002\u0002\u0006B\u0003",
    "\u0002\u0002\u0002\bk\u0003\u0002\u0002\u0002\nm\u0003\u0002\u0002\u0002",
    "\fo\u0003\u0002\u0002\u0002\u000eq\u0003\u0002\u0002\u0002\u0010u\u0003",
    "\u0002\u0002\u0002\u0012w\u0003\u0002\u0002\u0002\u0014y\u0003\u0002",
    "\u0002\u0002\u0016~\u0003\u0002\u0002\u0002\u0018\u0080\u0003\u0002",
    "\u0002\u0002\u001a\u001d\u0005\u0004\u0003\u0002\u001b\u001d\u0005\u0006",
    "\u0004\u0002\u001c\u001a\u0003\u0002\u0002\u0002\u001c\u001b\u0003\u0002",
    "\u0002\u0002\u001d \u0003\u0002\u0002\u0002\u001e\u001c\u0003\u0002",
    "\u0002\u0002\u001e\u001f\u0003\u0002\u0002\u0002\u001f\u0003\u0003\u0002",
    "\u0002\u0002 \u001e\u0003\u0002\u0002\u0002!\"\u0007\u0003\u0002\u0002",
    "\"$\u0005\u0012\n\u0002#%\u0007\u0004\u0002\u0002$#\u0003\u0002\u0002",
    "\u0002$%\u0003\u0002\u0002\u0002%\u0005\u0003\u0002\u0002\u0002&\'\u0007",
    "\u0014\u0002\u0002\'(\u0007\u0005\u0002\u0002()\u0005\n\u0006\u0002",
    ")-\u0007\u0006\u0002\u0002*,\u0005\u0010\t\u0002+*\u0003\u0002\u0002",
    "\u0002,/\u0003\u0002\u0002\u0002-+\u0003\u0002\u0002\u0002-.\u0003\u0002",
    "\u0002\u0002.0\u0003\u0002\u0002\u0002/-\u0003\u0002\u0002\u000202\u0007",
    "\u0007\u0002\u000213\u0007\u0004\u0002\u000221\u0003\u0002\u0002\u0002",
    "23\u0003\u0002\u0002\u00023C\u0003\u0002\u0002\u000245\u0007\u0014\u0002",
    "\u000256\u0007\u0005\u0002\u000267\u0005\f\u0007\u00027;\u0007\u0006",
    "\u0002\u00028:\u0005\u0014\u000b\u000298\u0003\u0002\u0002\u0002:=\u0003",
    "\u0002\u0002\u0002;9\u0003\u0002\u0002\u0002;<\u0003\u0002\u0002\u0002",
    "<>\u0003\u0002\u0002\u0002=;\u0003\u0002\u0002\u0002>@\u0007\u0007\u0002",
    "\u0002?A\u0007\u0004\u0002\u0002@?\u0003\u0002\u0002\u0002@A\u0003\u0002",
    "\u0002\u0002AC\u0003\u0002\u0002\u0002B&\u0003\u0002\u0002\u0002B4\u0003",
    "\u0002\u0002\u0002C\u0007\u0003\u0002\u0002\u0002DE\u0007\u0005\u0002",
    "\u0002EF\u0005\n\u0006\u0002FJ\u0007\u0006\u0002\u0002GI\u0005\u0010",
    "\t\u0002HG\u0003\u0002\u0002\u0002IL\u0003\u0002\u0002\u0002JH\u0003",
    "\u0002\u0002\u0002JK\u0003\u0002\u0002\u0002KM\u0003\u0002\u0002\u0002",
    "LJ\u0003\u0002\u0002\u0002MO\u0007\u0007\u0002\u0002NP\u0007\u0004\u0002",
    "\u0002ON\u0003\u0002\u0002\u0002OP\u0003\u0002\u0002\u0002Pl\u0003\u0002",
    "\u0002\u0002QR\u0007\u0005\u0002\u0002RS\u0005\f\u0007\u0002SW\u0007",
    "\u0006\u0002\u0002TV\u0005\u0014\u000b\u0002UT\u0003\u0002\u0002\u0002",
    "VY\u0003\u0002\u0002\u0002WU\u0003\u0002\u0002\u0002WX\u0003\u0002\u0002",
    "\u0002XZ\u0003\u0002\u0002\u0002YW\u0003\u0002\u0002\u0002Z\\\u0007",
    "\u0007\u0002\u0002[]\u0007\u0004\u0002\u0002\\[\u0003\u0002\u0002\u0002",
    "\\]\u0003\u0002\u0002\u0002]l\u0003\u0002\u0002\u0002^_\u0007\u0005",
    "\u0002\u0002_`\u0005\u000e\b\u0002`d\u0007\u0006\u0002\u0002ac\u0005",
    "\u0014\u000b\u0002ba\u0003\u0002\u0002\u0002cf\u0003\u0002\u0002\u0002",
    "db\u0003\u0002\u0002\u0002de\u0003\u0002\u0002\u0002eg\u0003\u0002\u0002",
    "\u0002fd\u0003\u0002\u0002\u0002gi\u0007\u0007\u0002\u0002hj\u0007\u0004",
    "\u0002\u0002ih\u0003\u0002\u0002\u0002ij\u0003\u0002\u0002\u0002jl\u0003",
    "\u0002\u0002\u0002kD\u0003\u0002\u0002\u0002kQ\u0003\u0002\u0002\u0002",
    "k^\u0003\u0002\u0002\u0002l\t\u0003\u0002\u0002\u0002mn\t\u0002\u0002",
    "\u0002n\u000b\u0003\u0002\u0002\u0002op\t\u0003\u0002\u0002p\r\u0003",
    "\u0002\u0002\u0002qr\u0007\u0014\u0002\u0002r\u000f\u0003\u0002\u0002",
    "\u0002sv\u0005\u0014\u000b\u0002tv\u0005\b\u0005\u0002us\u0003\u0002",
    "\u0002\u0002ut\u0003\u0002\u0002\u0002v\u0011\u0003\u0002\u0002\u0002",
    "wx\u0007\u0012\u0002\u0002x\u0013\u0003\u0002\u0002\u0002yz\u0005\u0016",
    "\f\u0002z{\u0007\u000e\u0002\u0002{|\u0005\u0018\r\u0002|}\u0007\u0004",
    "\u0002\u0002}\u0015\u0003\u0002\u0002\u0002~\u007f\u0007\u0014\u0002",
    "\u0002\u007f\u0017\u0003\u0002\u0002\u0002\u0080\u0081\t\u0004\u0002",
    "\u0002\u0081\u0019\u0003\u0002\u0002\u0002\u0012\u001c\u001e$-2;@BJ",
    "OW\\diku"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'@include'", "';'", "':'", "'{'", "'}'", "'dialog'", 
                     "'row'", "'column'", "'text'", "'button'", "'edit_box'", 
                     "'='" ];

var symbolicNames = [ null, null, null, null, null, null, null, null, null, 
                      null, null, null, null, "BOOL", "INT", "REAL", "STR", 
                      "ALIGN", "ID", "COMMENT", "LINE_COMMENT", "NEWLINE", 
                      "WHITESPACE" ];

var ruleNames =  [ "file", "includeFile", "defineTile", "innerTile", "clusterTile", 
                   "simpleTile", "deriveTile", "entry", "fileName", "attribute", 
                   "attributeName", "attributeValue" ];

function VeDclParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

VeDclParser.prototype = Object.create(antlr4.Parser.prototype);
VeDclParser.prototype.constructor = VeDclParser;

Object.defineProperty(VeDclParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

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
VeDclParser.BOOL = 13;
VeDclParser.INT = 14;
VeDclParser.REAL = 15;
VeDclParser.STR = 16;
VeDclParser.ALIGN = 17;
VeDclParser.ID = 18;
VeDclParser.COMMENT = 19;
VeDclParser.LINE_COMMENT = 20;
VeDclParser.NEWLINE = 21;
VeDclParser.WHITESPACE = 22;

VeDclParser.RULE_file = 0;
VeDclParser.RULE_includeFile = 1;
VeDclParser.RULE_defineTile = 2;
VeDclParser.RULE_innerTile = 3;
VeDclParser.RULE_clusterTile = 4;
VeDclParser.RULE_simpleTile = 5;
VeDclParser.RULE_deriveTile = 6;
VeDclParser.RULE_entry = 7;
VeDclParser.RULE_fileName = 8;
VeDclParser.RULE_attribute = 9;
VeDclParser.RULE_attributeName = 10;
VeDclParser.RULE_attributeValue = 11;


function FileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_file;
    return this;
}

FileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FileContext.prototype.constructor = FileContext;

FileContext.prototype.includeFile = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IncludeFileContext);
    } else {
        return this.getTypedRuleContext(IncludeFileContext,i);
    }
};

FileContext.prototype.defineTile = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DefineTileContext);
    } else {
        return this.getTypedRuleContext(DefineTileContext,i);
    }
};

FileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterFile(this);
	}
};

FileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitFile(this);
	}
};




VeDclParser.FileContext = FileContext;

VeDclParser.prototype.file = function() {

    var localctx = new FileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, VeDclParser.RULE_file);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 28;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
            this.state = 26;
            this._errHandler.sync(this);
            switch(this._input.LA(1)) {
            case VeDclParser.T__0:
                this.state = 24;
                this.includeFile();
                break;
            case VeDclParser.ID:
                this.state = 25;
                this.defineTile();
                break;
            default:
                throw new antlr4.error.NoViableAltException(this);
            }
            this.state = 30;
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
};


function IncludeFileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_includeFile;
    return this;
}

IncludeFileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IncludeFileContext.prototype.constructor = IncludeFileContext;

IncludeFileContext.prototype.fileName = function() {
    return this.getTypedRuleContext(FileNameContext,0);
};

IncludeFileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterIncludeFile(this);
	}
};

IncludeFileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitIncludeFile(this);
	}
};




VeDclParser.IncludeFileContext = IncludeFileContext;

VeDclParser.prototype.includeFile = function() {

    var localctx = new IncludeFileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, VeDclParser.RULE_includeFile);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 31;
        this.match(VeDclParser.T__0);
        this.state = 32;
        this.fileName();
        this.state = 34;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===VeDclParser.T__1) {
            this.state = 33;
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
};


function DefineTileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_defineTile;
    return this;
}

DefineTileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DefineTileContext.prototype.constructor = DefineTileContext;


 
DefineTileContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};


function DefineClusterTileContext(parser, ctx) {
	DefineTileContext.call(this, parser);
    DefineTileContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DefineClusterTileContext.prototype = Object.create(DefineTileContext.prototype);
DefineClusterTileContext.prototype.constructor = DefineClusterTileContext;

VeDclParser.DefineClusterTileContext = DefineClusterTileContext;

DefineClusterTileContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

DefineClusterTileContext.prototype.clusterTile = function() {
    return this.getTypedRuleContext(ClusterTileContext,0);
};

DefineClusterTileContext.prototype.entry = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(EntryContext);
    } else {
        return this.getTypedRuleContext(EntryContext,i);
    }
};
DefineClusterTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterDefineClusterTile(this);
	}
};

DefineClusterTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitDefineClusterTile(this);
	}
};


function DefineSimpleTileContext(parser, ctx) {
	DefineTileContext.call(this, parser);
    DefineTileContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DefineSimpleTileContext.prototype = Object.create(DefineTileContext.prototype);
DefineSimpleTileContext.prototype.constructor = DefineSimpleTileContext;

VeDclParser.DefineSimpleTileContext = DefineSimpleTileContext;

DefineSimpleTileContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

DefineSimpleTileContext.prototype.simpleTile = function() {
    return this.getTypedRuleContext(SimpleTileContext,0);
};

DefineSimpleTileContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
DefineSimpleTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterDefineSimpleTile(this);
	}
};

DefineSimpleTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitDefineSimpleTile(this);
	}
};



VeDclParser.DefineTileContext = DefineTileContext;

VeDclParser.prototype.defineTile = function() {

    var localctx = new DefineTileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, VeDclParser.RULE_defineTile);
    var _la = 0; // Token type
    try {
        this.state = 64;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,7,this._ctx);
        switch(la_) {
        case 1:
            localctx = new DefineClusterTileContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 36;
            this.match(VeDclParser.ID);
            this.state = 37;
            this.match(VeDclParser.T__2);
            this.state = 38;
            this.clusterTile();
            this.state = 39;
            this.match(VeDclParser.T__3);
            this.state = 43;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__2 || _la===VeDclParser.ID) {
                this.state = 40;
                this.entry();
                this.state = 45;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 46;
            this.match(VeDclParser.T__4);
            this.state = 48;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__1) {
                this.state = 47;
                this.match(VeDclParser.T__1);
            }

            break;

        case 2:
            localctx = new DefineSimpleTileContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 50;
            this.match(VeDclParser.ID);
            this.state = 51;
            this.match(VeDclParser.T__2);
            this.state = 52;
            this.simpleTile();
            this.state = 53;
            this.match(VeDclParser.T__3);
            this.state = 57;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 54;
                this.attribute();
                this.state = 59;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 60;
            this.match(VeDclParser.T__4);
            this.state = 62;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__1) {
                this.state = 61;
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
};


function InnerTileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_innerTile;
    return this;
}

InnerTileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
InnerTileContext.prototype.constructor = InnerTileContext;


 
InnerTileContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};


function InnerSimpleTileContext(parser, ctx) {
	InnerTileContext.call(this, parser);
    InnerTileContext.prototype.copyFrom.call(this, ctx);
    return this;
}

InnerSimpleTileContext.prototype = Object.create(InnerTileContext.prototype);
InnerSimpleTileContext.prototype.constructor = InnerSimpleTileContext;

VeDclParser.InnerSimpleTileContext = InnerSimpleTileContext;

InnerSimpleTileContext.prototype.simpleTile = function() {
    return this.getTypedRuleContext(SimpleTileContext,0);
};

InnerSimpleTileContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
InnerSimpleTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterInnerSimpleTile(this);
	}
};

InnerSimpleTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitInnerSimpleTile(this);
	}
};


function InnerDeriveTileContext(parser, ctx) {
	InnerTileContext.call(this, parser);
    InnerTileContext.prototype.copyFrom.call(this, ctx);
    return this;
}

InnerDeriveTileContext.prototype = Object.create(InnerTileContext.prototype);
InnerDeriveTileContext.prototype.constructor = InnerDeriveTileContext;

VeDclParser.InnerDeriveTileContext = InnerDeriveTileContext;

InnerDeriveTileContext.prototype.deriveTile = function() {
    return this.getTypedRuleContext(DeriveTileContext,0);
};

InnerDeriveTileContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
InnerDeriveTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterInnerDeriveTile(this);
	}
};

InnerDeriveTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitInnerDeriveTile(this);
	}
};


function InnerClusterTileContext(parser, ctx) {
	InnerTileContext.call(this, parser);
    InnerTileContext.prototype.copyFrom.call(this, ctx);
    return this;
}

InnerClusterTileContext.prototype = Object.create(InnerTileContext.prototype);
InnerClusterTileContext.prototype.constructor = InnerClusterTileContext;

VeDclParser.InnerClusterTileContext = InnerClusterTileContext;

InnerClusterTileContext.prototype.clusterTile = function() {
    return this.getTypedRuleContext(ClusterTileContext,0);
};

InnerClusterTileContext.prototype.entry = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(EntryContext);
    } else {
        return this.getTypedRuleContext(EntryContext,i);
    }
};
InnerClusterTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterInnerClusterTile(this);
	}
};

InnerClusterTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitInnerClusterTile(this);
	}
};



VeDclParser.InnerTileContext = InnerTileContext;

VeDclParser.prototype.innerTile = function() {

    var localctx = new InnerTileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, VeDclParser.RULE_innerTile);
    var _la = 0; // Token type
    try {
        this.state = 105;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,14,this._ctx);
        switch(la_) {
        case 1:
            localctx = new InnerClusterTileContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 66;
            this.match(VeDclParser.T__2);
            this.state = 67;
            this.clusterTile();
            this.state = 68;
            this.match(VeDclParser.T__3);
            this.state = 72;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__2 || _la===VeDclParser.ID) {
                this.state = 69;
                this.entry();
                this.state = 74;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 75;
            this.match(VeDclParser.T__4);
            this.state = 77;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__1) {
                this.state = 76;
                this.match(VeDclParser.T__1);
            }

            break;

        case 2:
            localctx = new InnerSimpleTileContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 79;
            this.match(VeDclParser.T__2);
            this.state = 80;
            this.simpleTile();
            this.state = 81;
            this.match(VeDclParser.T__3);
            this.state = 85;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 82;
                this.attribute();
                this.state = 87;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 88;
            this.match(VeDclParser.T__4);
            this.state = 90;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__1) {
                this.state = 89;
                this.match(VeDclParser.T__1);
            }

            break;

        case 3:
            localctx = new InnerDeriveTileContext(this, localctx);
            this.enterOuterAlt(localctx, 3);
            this.state = 92;
            this.match(VeDclParser.T__2);
            this.state = 93;
            this.deriveTile();
            this.state = 94;
            this.match(VeDclParser.T__3);
            this.state = 98;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 95;
                this.attribute();
                this.state = 100;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 101;
            this.match(VeDclParser.T__4);
            this.state = 103;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__1) {
                this.state = 102;
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
};


function ClusterTileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_clusterTile;
    return this;
}

ClusterTileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ClusterTileContext.prototype.constructor = ClusterTileContext;


ClusterTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterClusterTile(this);
	}
};

ClusterTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitClusterTile(this);
	}
};




VeDclParser.ClusterTileContext = ClusterTileContext;

VeDclParser.prototype.clusterTile = function() {

    var localctx = new ClusterTileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 8, VeDclParser.RULE_clusterTile);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 107;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__5) | (1 << VeDclParser.T__6) | (1 << VeDclParser.T__7))) !== 0))) {
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
};


function SimpleTileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_simpleTile;
    return this;
}

SimpleTileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
SimpleTileContext.prototype.constructor = SimpleTileContext;


SimpleTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterSimpleTile(this);
	}
};

SimpleTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitSimpleTile(this);
	}
};




VeDclParser.SimpleTileContext = SimpleTileContext;

VeDclParser.prototype.simpleTile = function() {

    var localctx = new SimpleTileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, VeDclParser.RULE_simpleTile);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 109;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__8) | (1 << VeDclParser.T__9) | (1 << VeDclParser.T__10))) !== 0))) {
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
};


function DeriveTileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_deriveTile;
    return this;
}

DeriveTileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DeriveTileContext.prototype.constructor = DeriveTileContext;

DeriveTileContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

DeriveTileContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterDeriveTile(this);
	}
};

DeriveTileContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitDeriveTile(this);
	}
};




VeDclParser.DeriveTileContext = DeriveTileContext;

VeDclParser.prototype.deriveTile = function() {

    var localctx = new DeriveTileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, VeDclParser.RULE_deriveTile);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 111;
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
};


function EntryContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_entry;
    return this;
}

EntryContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
EntryContext.prototype.constructor = EntryContext;

EntryContext.prototype.attribute = function() {
    return this.getTypedRuleContext(AttributeContext,0);
};

EntryContext.prototype.innerTile = function() {
    return this.getTypedRuleContext(InnerTileContext,0);
};

EntryContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterEntry(this);
	}
};

EntryContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitEntry(this);
	}
};




VeDclParser.EntryContext = EntryContext;

VeDclParser.prototype.entry = function() {

    var localctx = new EntryContext(this, this._ctx, this.state);
    this.enterRule(localctx, 14, VeDclParser.RULE_entry);
    try {
        this.state = 115;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case VeDclParser.ID:
            this.enterOuterAlt(localctx, 1);
            this.state = 113;
            this.attribute();
            break;
        case VeDclParser.T__2:
            this.enterOuterAlt(localctx, 2);
            this.state = 114;
            this.innerTile();
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
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


function FileNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_fileName;
    return this;
}

FileNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FileNameContext.prototype.constructor = FileNameContext;

FileNameContext.prototype.STR = function() {
    return this.getToken(VeDclParser.STR, 0);
};

FileNameContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterFileName(this);
	}
};

FileNameContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitFileName(this);
	}
};




VeDclParser.FileNameContext = FileNameContext;

VeDclParser.prototype.fileName = function() {

    var localctx = new FileNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 16, VeDclParser.RULE_fileName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 117;
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
};


function AttributeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_attribute;
    return this;
}

AttributeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AttributeContext.prototype.constructor = AttributeContext;

AttributeContext.prototype.attributeName = function() {
    return this.getTypedRuleContext(AttributeNameContext,0);
};

AttributeContext.prototype.attributeValue = function() {
    return this.getTypedRuleContext(AttributeValueContext,0);
};

AttributeContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterAttribute(this);
	}
};

AttributeContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitAttribute(this);
	}
};




VeDclParser.AttributeContext = AttributeContext;

VeDclParser.prototype.attribute = function() {

    var localctx = new AttributeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, VeDclParser.RULE_attribute);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 119;
        this.attributeName();
        this.state = 120;
        this.match(VeDclParser.T__11);
        this.state = 121;
        this.attributeValue();
        this.state = 122;
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
};


function AttributeNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_attributeName;
    return this;
}

AttributeNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AttributeNameContext.prototype.constructor = AttributeNameContext;

AttributeNameContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

AttributeNameContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterAttributeName(this);
	}
};

AttributeNameContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitAttributeName(this);
	}
};




VeDclParser.AttributeNameContext = AttributeNameContext;

VeDclParser.prototype.attributeName = function() {

    var localctx = new AttributeNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 20, VeDclParser.RULE_attributeName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 124;
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
};


function AttributeValueContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_attributeValue;
    return this;
}

AttributeValueContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AttributeValueContext.prototype.constructor = AttributeValueContext;

AttributeValueContext.prototype.BOOL = function() {
    return this.getToken(VeDclParser.BOOL, 0);
};

AttributeValueContext.prototype.INT = function() {
    return this.getToken(VeDclParser.INT, 0);
};

AttributeValueContext.prototype.REAL = function() {
    return this.getToken(VeDclParser.REAL, 0);
};

AttributeValueContext.prototype.STR = function() {
    return this.getToken(VeDclParser.STR, 0);
};

AttributeValueContext.prototype.ALIGN = function() {
    return this.getToken(VeDclParser.ALIGN, 0);
};

AttributeValueContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterAttributeValue(this);
	}
};

AttributeValueContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitAttributeValue(this);
	}
};




VeDclParser.AttributeValueContext = AttributeValueContext;

VeDclParser.prototype.attributeValue = function() {

    var localctx = new AttributeValueContext(this, this._ctx, this.state);
    this.enterRule(localctx, 22, VeDclParser.RULE_attributeValue);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 126;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.BOOL) | (1 << VeDclParser.INT) | (1 << VeDclParser.REAL) | (1 << VeDclParser.STR) | (1 << VeDclParser.ALIGN))) !== 0))) {
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
};


exports.VeDclParser = VeDclParser;
