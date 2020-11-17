// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeDclListener = require('./VeDclListener').VeDclListener;
var grammarFileName = "VeDcl.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u0017w\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004\t",
    "\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007\u0004",
    "\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0003\u0002\u0007",
    "\u0002\u0018\n\u0002\f\u0002\u000e\u0002\u001b\u000b\u0002\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\"\n\u0003",
    "\f\u0003\u000e\u0003%\u000b\u0003\u0003\u0003\u0003\u0003\u0005\u0003",
    ")\n\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u00030\n\u0003\f\u0003\u000e\u00033\u000b\u0003\u0003\u0003\u0003",
    "\u0003\u0005\u00037\n\u0003\u0005\u00039\n\u0003\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0004\u0007\u0004?\n\u0004\f\u0004\u000e\u0004B\u000b",
    "\u0004\u0003\u0004\u0003\u0004\u0005\u0004F\n\u0004\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0003\u0004\u0007\u0004L\n\u0004\f\u0004\u000e\u0004",
    "O\u000b\u0004\u0003\u0004\u0003\u0004\u0005\u0004S\n\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0004\u0003\u0004\u0007\u0004Y\n\u0004\f\u0004\u000e",
    "\u0004\\\u000b\u0004\u0003\u0004\u0003\u0004\u0005\u0004`\n\u0004\u0005",
    "\u0004b\n\u0004\u0003\u0005\u0003\u0005\u0003\u0006\u0003\u0006\u0003",
    "\u0007\u0003\u0007\u0003\b\u0003\b\u0005\bl\n\b\u0003\t\u0003\t\u0003",
    "\t\u0003\t\u0003\t\u0003\n\u0003\n\u0003\u000b\u0003\u000b\u0003\u000b",
    "\u0002\u0002\f\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0002",
    "\u0005\u0003\u0002\u0007\t\u0003\u0002\n\f\u0003\u0002\u000e\u0012\u0002",
    "{\u0002\u0019\u0003\u0002\u0002\u0002\u00048\u0003\u0002\u0002\u0002",
    "\u0006a\u0003\u0002\u0002\u0002\bc\u0003\u0002\u0002\u0002\ne\u0003",
    "\u0002\u0002\u0002\fg\u0003\u0002\u0002\u0002\u000ek\u0003\u0002\u0002",
    "\u0002\u0010m\u0003\u0002\u0002\u0002\u0012r\u0003\u0002\u0002\u0002",
    "\u0014t\u0003\u0002\u0002\u0002\u0016\u0018\u0005\u0004\u0003\u0002",
    "\u0017\u0016\u0003\u0002\u0002\u0002\u0018\u001b\u0003\u0002\u0002\u0002",
    "\u0019\u0017\u0003\u0002\u0002\u0002\u0019\u001a\u0003\u0002\u0002\u0002",
    "\u001a\u0003\u0003\u0002\u0002\u0002\u001b\u0019\u0003\u0002\u0002\u0002",
    "\u001c\u001d\u0007\u0013\u0002\u0002\u001d\u001e\u0007\u0003\u0002\u0002",
    "\u001e\u001f\u0005\b\u0005\u0002\u001f#\u0007\u0004\u0002\u0002 \"\u0005",
    "\u000e\b\u0002! \u0003\u0002\u0002\u0002\"%\u0003\u0002\u0002\u0002",
    "#!\u0003\u0002\u0002\u0002#$\u0003\u0002\u0002\u0002$&\u0003\u0002\u0002",
    "\u0002%#\u0003\u0002\u0002\u0002&(\u0007\u0005\u0002\u0002\')\u0007",
    "\u0006\u0002\u0002(\'\u0003\u0002\u0002\u0002()\u0003\u0002\u0002\u0002",
    ")9\u0003\u0002\u0002\u0002*+\u0007\u0013\u0002\u0002+,\u0007\u0003\u0002",
    "\u0002,-\u0005\n\u0006\u0002-1\u0007\u0004\u0002\u0002.0\u0005\u0010",
    "\t\u0002/.\u0003\u0002\u0002\u000203\u0003\u0002\u0002\u00021/\u0003",
    "\u0002\u0002\u000212\u0003\u0002\u0002\u000224\u0003\u0002\u0002\u0002",
    "31\u0003\u0002\u0002\u000246\u0007\u0005\u0002\u000257\u0007\u0006\u0002",
    "\u000265\u0003\u0002\u0002\u000267\u0003\u0002\u0002\u000279\u0003\u0002",
    "\u0002\u00028\u001c\u0003\u0002\u0002\u00028*\u0003\u0002\u0002\u0002",
    "9\u0005\u0003\u0002\u0002\u0002:;\u0007\u0003\u0002\u0002;<\u0005\b",
    "\u0005\u0002<@\u0007\u0004\u0002\u0002=?\u0005\u000e\b\u0002>=\u0003",
    "\u0002\u0002\u0002?B\u0003\u0002\u0002\u0002@>\u0003\u0002\u0002\u0002",
    "@A\u0003\u0002\u0002\u0002AC\u0003\u0002\u0002\u0002B@\u0003\u0002\u0002",
    "\u0002CE\u0007\u0005\u0002\u0002DF\u0007\u0006\u0002\u0002ED\u0003\u0002",
    "\u0002\u0002EF\u0003\u0002\u0002\u0002Fb\u0003\u0002\u0002\u0002GH\u0007",
    "\u0003\u0002\u0002HI\u0005\n\u0006\u0002IM\u0007\u0004\u0002\u0002J",
    "L\u0005\u0010\t\u0002KJ\u0003\u0002\u0002\u0002LO\u0003\u0002\u0002",
    "\u0002MK\u0003\u0002\u0002\u0002MN\u0003\u0002\u0002\u0002NP\u0003\u0002",
    "\u0002\u0002OM\u0003\u0002\u0002\u0002PR\u0007\u0005\u0002\u0002QS\u0007",
    "\u0006\u0002\u0002RQ\u0003\u0002\u0002\u0002RS\u0003\u0002\u0002\u0002",
    "Sb\u0003\u0002\u0002\u0002TU\u0007\u0003\u0002\u0002UV\u0005\f\u0007",
    "\u0002VZ\u0007\u0004\u0002\u0002WY\u0005\u0010\t\u0002XW\u0003\u0002",
    "\u0002\u0002Y\\\u0003\u0002\u0002\u0002ZX\u0003\u0002\u0002\u0002Z[",
    "\u0003\u0002\u0002\u0002[]\u0003\u0002\u0002\u0002\\Z\u0003\u0002\u0002",
    "\u0002]_\u0007\u0005\u0002\u0002^`\u0007\u0006\u0002\u0002_^\u0003\u0002",
    "\u0002\u0002_`\u0003\u0002\u0002\u0002`b\u0003\u0002\u0002\u0002a:\u0003",
    "\u0002\u0002\u0002aG\u0003\u0002\u0002\u0002aT\u0003\u0002\u0002\u0002",
    "b\u0007\u0003\u0002\u0002\u0002cd\t\u0002\u0002\u0002d\t\u0003\u0002",
    "\u0002\u0002ef\t\u0003\u0002\u0002f\u000b\u0003\u0002\u0002\u0002gh",
    "\u0007\u0013\u0002\u0002h\r\u0003\u0002\u0002\u0002il\u0005\u0010\t",
    "\u0002jl\u0005\u0006\u0004\u0002ki\u0003\u0002\u0002\u0002kj\u0003\u0002",
    "\u0002\u0002l\u000f\u0003\u0002\u0002\u0002mn\u0005\u0012\n\u0002no",
    "\u0007\r\u0002\u0002op\u0005\u0014\u000b\u0002pq\u0007\u0006\u0002\u0002",
    "q\u0011\u0003\u0002\u0002\u0002rs\u0007\u0013\u0002\u0002s\u0013\u0003",
    "\u0002\u0002\u0002tu\t\u0004\u0002\u0002u\u0015\u0003\u0002\u0002\u0002",
    "\u0010\u0019#(168@EMRZ_ak"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "':'", "'{'", "'}'", "';'", "'dialog'", "'row'", 
                     "'column'", "'text'", "'button'", "'edit_box'", "'='" ];

var symbolicNames = [ null, null, null, null, null, null, null, null, null, 
                      null, null, null, "BOOL", "INT", "REAL", "STR", "ALIGN", 
                      "ID", "COMMENT", "LINE_COMMENT", "NEWLINE", "WHITESPACE" ];

var ruleNames =  [ "file", "defineTile", "innerTile", "clusterTile", "simpleTile", 
                   "deriveTile", "entry", "attribute", "attributeName", 
                   "attributeValue" ];

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
VeDclParser.BOOL = 12;
VeDclParser.INT = 13;
VeDclParser.REAL = 14;
VeDclParser.STR = 15;
VeDclParser.ALIGN = 16;
VeDclParser.ID = 17;
VeDclParser.COMMENT = 18;
VeDclParser.LINE_COMMENT = 19;
VeDclParser.NEWLINE = 20;
VeDclParser.WHITESPACE = 21;

VeDclParser.RULE_file = 0;
VeDclParser.RULE_defineTile = 1;
VeDclParser.RULE_innerTile = 2;
VeDclParser.RULE_clusterTile = 3;
VeDclParser.RULE_simpleTile = 4;
VeDclParser.RULE_deriveTile = 5;
VeDclParser.RULE_entry = 6;
VeDclParser.RULE_attribute = 7;
VeDclParser.RULE_attributeName = 8;
VeDclParser.RULE_attributeValue = 9;


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
        this.state = 23;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.ID) {
            this.state = 20;
            this.defineTile();
            this.state = 25;
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
    this.enterRule(localctx, 2, VeDclParser.RULE_defineTile);
    var _la = 0; // Token type
    try {
        this.state = 54;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,5,this._ctx);
        switch(la_) {
        case 1:
            localctx = new DefineClusterTileContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 26;
            this.match(VeDclParser.ID);
            this.state = 27;
            this.match(VeDclParser.T__0);
            this.state = 28;
            this.clusterTile();
            this.state = 29;
            this.match(VeDclParser.T__1);
            this.state = 33;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
                this.state = 30;
                this.entry();
                this.state = 35;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 36;
            this.match(VeDclParser.T__2);
            this.state = 38;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__3) {
                this.state = 37;
                this.match(VeDclParser.T__3);
            }

            break;

        case 2:
            localctx = new DefineSimpleTileContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 40;
            this.match(VeDclParser.ID);
            this.state = 41;
            this.match(VeDclParser.T__0);
            this.state = 42;
            this.simpleTile();
            this.state = 43;
            this.match(VeDclParser.T__1);
            this.state = 47;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 44;
                this.attribute();
                this.state = 49;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 50;
            this.match(VeDclParser.T__2);
            this.state = 52;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__3) {
                this.state = 51;
                this.match(VeDclParser.T__3);
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
    this.enterRule(localctx, 4, VeDclParser.RULE_innerTile);
    var _la = 0; // Token type
    try {
        this.state = 95;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,12,this._ctx);
        switch(la_) {
        case 1:
            localctx = new InnerClusterTileContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 56;
            this.match(VeDclParser.T__0);
            this.state = 57;
            this.clusterTile();
            this.state = 58;
            this.match(VeDclParser.T__1);
            this.state = 62;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
                this.state = 59;
                this.entry();
                this.state = 64;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 65;
            this.match(VeDclParser.T__2);
            this.state = 67;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__3) {
                this.state = 66;
                this.match(VeDclParser.T__3);
            }

            break;

        case 2:
            localctx = new InnerSimpleTileContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 69;
            this.match(VeDclParser.T__0);
            this.state = 70;
            this.simpleTile();
            this.state = 71;
            this.match(VeDclParser.T__1);
            this.state = 75;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 72;
                this.attribute();
                this.state = 77;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 78;
            this.match(VeDclParser.T__2);
            this.state = 80;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__3) {
                this.state = 79;
                this.match(VeDclParser.T__3);
            }

            break;

        case 3:
            localctx = new InnerDeriveTileContext(this, localctx);
            this.enterOuterAlt(localctx, 3);
            this.state = 82;
            this.match(VeDclParser.T__0);
            this.state = 83;
            this.deriveTile();
            this.state = 84;
            this.match(VeDclParser.T__1);
            this.state = 88;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 85;
                this.attribute();
                this.state = 90;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 91;
            this.match(VeDclParser.T__2);
            this.state = 93;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__3) {
                this.state = 92;
                this.match(VeDclParser.T__3);
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
    this.enterRule(localctx, 6, VeDclParser.RULE_clusterTile);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 97;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__4) | (1 << VeDclParser.T__5) | (1 << VeDclParser.T__6))) !== 0))) {
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
    this.enterRule(localctx, 8, VeDclParser.RULE_simpleTile);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 99;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeDclParser.T__7) | (1 << VeDclParser.T__8) | (1 << VeDclParser.T__9))) !== 0))) {
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
    this.enterRule(localctx, 10, VeDclParser.RULE_deriveTile);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 101;
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
    this.enterRule(localctx, 12, VeDclParser.RULE_entry);
    try {
        this.state = 105;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case VeDclParser.ID:
            this.enterOuterAlt(localctx, 1);
            this.state = 103;
            this.attribute();
            break;
        case VeDclParser.T__0:
            this.enterOuterAlt(localctx, 2);
            this.state = 104;
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
    this.enterRule(localctx, 14, VeDclParser.RULE_attribute);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 107;
        this.attributeName();
        this.state = 108;
        this.match(VeDclParser.T__10);
        this.state = 109;
        this.attributeValue();
        this.state = 110;
        this.match(VeDclParser.T__3);
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
    this.enterRule(localctx, 16, VeDclParser.RULE_attributeName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 112;
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
    this.enterRule(localctx, 18, VeDclParser.RULE_attributeValue);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 114;
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
