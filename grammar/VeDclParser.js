// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeDclListener = require('./VeDclListener').VeDclListener;
var grammarFileName = "VeDcl.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u0017\u0084\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0003\u0002\u0007\u0002\u0012\n\u0002\f\u0002\u000e\u0002",
    "\u0015\u000b\u0002\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003\u001c\n\u0003\f\u0003\u000e\u0003\u001f\u000b\u0003",
    "\u0003\u0003\u0003\u0003\u0005\u0003#\n\u0003\u0003\u0004\u0003\u0004",
    "\u0005\u0004\'\n\u0004\u0003\u0005\u0005\u0005*\n\u0005\u0003\u0005",
    "\u0003\u0005\u0003\u0005\u0003\u0005\u0007\u00050\n\u0005\f\u0005\u000e",
    "\u00053\u000b\u0005\u0003\u0005\u0003\u0005\u0005\u00057\n\u0005\u0003",
    "\u0005\u0005\u0005:\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003",
    "\u0005\u0007\u0005@\n\u0005\f\u0005\u000e\u0005C\u000b\u0005\u0003\u0005",
    "\u0003\u0005\u0005\u0005G\n\u0005\u0003\u0005\u0005\u0005J\n\u0005\u0003",
    "\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0007\u0005P\n\u0005\f\u0005",
    "\u000e\u0005S\u000b\u0005\u0003\u0005\u0003\u0005\u0005\u0005W\n\u0005",
    "\u0003\u0005\u0005\u0005Z\n\u0005\u0003\u0005\u0003\u0005\u0003\u0005",
    "\u0003\u0005\u0007\u0005`\n\u0005\f\u0005\u000e\u0005c\u000b\u0005\u0003",
    "\u0005\u0003\u0005\u0005\u0005g\n\u0005\u0003\u0005\u0005\u0005j\n\u0005",
    "\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0007\u0005p\n\u0005",
    "\f\u0005\u000e\u0005s\u000b\u0005\u0003\u0005\u0003\u0005\u0005\u0005",
    "w\n\u0005\u0005\u0005y\n\u0005\u0003\u0006\u0003\u0006\u0003\u0006\u0003",
    "\u0006\u0003\u0006\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\b\u0002",
    "\u0002\t\u0002\u0004\u0006\b\n\f\u000e\u0002\u0003\u0003\u0002\u000e",
    "\u0012\u0002\u0093\u0002\u0013\u0003\u0002\u0002\u0002\u0004\u0016\u0003",
    "\u0002\u0002\u0002\u0006&\u0003\u0002\u0002\u0002\bx\u0003\u0002\u0002",
    "\u0002\nz\u0003\u0002\u0002\u0002\f\u007f\u0003\u0002\u0002\u0002\u000e",
    "\u0081\u0003\u0002\u0002\u0002\u0010\u0012\u0005\u0004\u0003\u0002\u0011",
    "\u0010\u0003\u0002\u0002\u0002\u0012\u0015\u0003\u0002\u0002\u0002\u0013",
    "\u0011\u0003\u0002\u0002\u0002\u0013\u0014\u0003\u0002\u0002\u0002\u0014",
    "\u0003\u0003\u0002\u0002\u0002\u0015\u0013\u0003\u0002\u0002\u0002\u0016",
    "\u0017\u0007\u0013\u0002\u0002\u0017\u0018\u0007\u0003\u0002\u0002\u0018",
    "\u0019\u0007\u0004\u0002\u0002\u0019\u001d\u0007\u0005\u0002\u0002\u001a",
    "\u001c\u0005\u0006\u0004\u0002\u001b\u001a\u0003\u0002\u0002\u0002\u001c",
    "\u001f\u0003\u0002\u0002\u0002\u001d\u001b\u0003\u0002\u0002\u0002\u001d",
    "\u001e\u0003\u0002\u0002\u0002\u001e \u0003\u0002\u0002\u0002\u001f",
    "\u001d\u0003\u0002\u0002\u0002 \"\u0007\u0006\u0002\u0002!#\u0007\u0007",
    "\u0002\u0002\"!\u0003\u0002\u0002\u0002\"#\u0003\u0002\u0002\u0002#",
    "\u0005\u0003\u0002\u0002\u0002$\'\u0005\n\u0006\u0002%\'\u0005\b\u0005",
    "\u0002&$\u0003\u0002\u0002\u0002&%\u0003\u0002\u0002\u0002\'\u0007\u0003",
    "\u0002\u0002\u0002(*\u0007\u0013\u0002\u0002)(\u0003\u0002\u0002\u0002",
    ")*\u0003\u0002\u0002\u0002*+\u0003\u0002\u0002\u0002+,\u0007\u0003\u0002",
    "\u0002,-\u0007\b\u0002\u0002-1\u0007\u0005\u0002\u0002.0\u0005\u0006",
    "\u0004\u0002/.\u0003\u0002\u0002\u000203\u0003\u0002\u0002\u00021/\u0003",
    "\u0002\u0002\u000212\u0003\u0002\u0002\u000224\u0003\u0002\u0002\u0002",
    "31\u0003\u0002\u0002\u000246\u0007\u0006\u0002\u000257\u0007\u0007\u0002",
    "\u000265\u0003\u0002\u0002\u000267\u0003\u0002\u0002\u00027y\u0003\u0002",
    "\u0002\u00028:\u0007\u0013\u0002\u000298\u0003\u0002\u0002\u00029:\u0003",
    "\u0002\u0002\u0002:;\u0003\u0002\u0002\u0002;<\u0007\u0003\u0002\u0002",
    "<=\u0007\t\u0002\u0002=A\u0007\u0005\u0002\u0002>@\u0005\u0006\u0004",
    "\u0002?>\u0003\u0002\u0002\u0002@C\u0003\u0002\u0002\u0002A?\u0003\u0002",
    "\u0002\u0002AB\u0003\u0002\u0002\u0002BD\u0003\u0002\u0002\u0002CA\u0003",
    "\u0002\u0002\u0002DF\u0007\u0006\u0002\u0002EG\u0007\u0007\u0002\u0002",
    "FE\u0003\u0002\u0002\u0002FG\u0003\u0002\u0002\u0002Gy\u0003\u0002\u0002",
    "\u0002HJ\u0007\u0013\u0002\u0002IH\u0003\u0002\u0002\u0002IJ\u0003\u0002",
    "\u0002\u0002JK\u0003\u0002\u0002\u0002KL\u0007\u0003\u0002\u0002LM\u0007",
    "\n\u0002\u0002MQ\u0007\u0005\u0002\u0002NP\u0005\n\u0006\u0002ON\u0003",
    "\u0002\u0002\u0002PS\u0003\u0002\u0002\u0002QO\u0003\u0002\u0002\u0002",
    "QR\u0003\u0002\u0002\u0002RT\u0003\u0002\u0002\u0002SQ\u0003\u0002\u0002",
    "\u0002TV\u0007\u0006\u0002\u0002UW\u0007\u0007\u0002\u0002VU\u0003\u0002",
    "\u0002\u0002VW\u0003\u0002\u0002\u0002Wy\u0003\u0002\u0002\u0002XZ\u0007",
    "\u0013\u0002\u0002YX\u0003\u0002\u0002\u0002YZ\u0003\u0002\u0002\u0002",
    "Z[\u0003\u0002\u0002\u0002[\\\u0007\u0003\u0002\u0002\\]\u0007\u000b",
    "\u0002\u0002]a\u0007\u0005\u0002\u0002^`\u0005\n\u0006\u0002_^\u0003",
    "\u0002\u0002\u0002`c\u0003\u0002\u0002\u0002a_\u0003\u0002\u0002\u0002",
    "ab\u0003\u0002\u0002\u0002bd\u0003\u0002\u0002\u0002ca\u0003\u0002\u0002",
    "\u0002df\u0007\u0006\u0002\u0002eg\u0007\u0007\u0002\u0002fe\u0003\u0002",
    "\u0002\u0002fg\u0003\u0002\u0002\u0002gy\u0003\u0002\u0002\u0002hj\u0007",
    "\u0013\u0002\u0002ih\u0003\u0002\u0002\u0002ij\u0003\u0002\u0002\u0002",
    "jk\u0003\u0002\u0002\u0002kl\u0007\u0003\u0002\u0002lm\u0007\f\u0002",
    "\u0002mq\u0007\u0005\u0002\u0002np\u0005\n\u0006\u0002on\u0003\u0002",
    "\u0002\u0002ps\u0003\u0002\u0002\u0002qo\u0003\u0002\u0002\u0002qr\u0003",
    "\u0002\u0002\u0002rt\u0003\u0002\u0002\u0002sq\u0003\u0002\u0002\u0002",
    "tv\u0007\u0006\u0002\u0002uw\u0007\u0007\u0002\u0002vu\u0003\u0002\u0002",
    "\u0002vw\u0003\u0002\u0002\u0002wy\u0003\u0002\u0002\u0002x)\u0003\u0002",
    "\u0002\u0002x9\u0003\u0002\u0002\u0002xI\u0003\u0002\u0002\u0002xY\u0003",
    "\u0002\u0002\u0002xi\u0003\u0002\u0002\u0002y\t\u0003\u0002\u0002\u0002",
    "z{\u0005\f\u0007\u0002{|\u0007\r\u0002\u0002|}\u0005\u000e\b\u0002}",
    "~\u0007\u0007\u0002\u0002~\u000b\u0003\u0002\u0002\u0002\u007f\u0080",
    "\u0007\u0013\u0002\u0002\u0080\r\u0003\u0002\u0002\u0002\u0081\u0082",
    "\t\u0002\u0002\u0002\u0082\u000f\u0003\u0002\u0002\u0002\u0016\u0013",
    "\u001d\"&)169AFIQVYafiqvx"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "':'", "'dialog'", "'{'", "'}'", "';'", "'row'", 
                     "'column'", "'text'", "'button'", "'edit_box'", "'='" ];

var symbolicNames = [ null, null, null, null, null, null, null, null, null, 
                      null, null, null, "BOOL", "INT", "REAL", "STR", "ALIGN", 
                      "ID", "COMMENT", "LINE_COMMENT", "NEWLINE", "WHITESPACE" ];

var ruleNames =  [ "file", "dialog", "entry", "control", "attribute", "attributeName", 
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
VeDclParser.RULE_dialog = 1;
VeDclParser.RULE_entry = 2;
VeDclParser.RULE_control = 3;
VeDclParser.RULE_attribute = 4;
VeDclParser.RULE_attributeName = 5;
VeDclParser.RULE_attributeValue = 6;


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

FileContext.prototype.dialog = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DialogContext);
    } else {
        return this.getTypedRuleContext(DialogContext,i);
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
        this.state = 17;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.ID) {
            this.state = 14;
            this.dialog();
            this.state = 19;
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


function DialogContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_dialog;
    return this;
}

DialogContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DialogContext.prototype.constructor = DialogContext;

DialogContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

DialogContext.prototype.entry = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(EntryContext);
    } else {
        return this.getTypedRuleContext(EntryContext,i);
    }
};

DialogContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterDialog(this);
	}
};

DialogContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitDialog(this);
	}
};




VeDclParser.DialogContext = DialogContext;

VeDclParser.prototype.dialog = function() {

    var localctx = new DialogContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, VeDclParser.RULE_dialog);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 20;
        this.match(VeDclParser.ID);
        this.state = 21;
        this.match(VeDclParser.T__0);
        this.state = 22;
        this.match(VeDclParser.T__1);
        this.state = 23;
        this.match(VeDclParser.T__2);
        this.state = 27;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
            this.state = 24;
            this.entry();
            this.state = 29;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 30;
        this.match(VeDclParser.T__3);
        this.state = 32;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===VeDclParser.T__4) {
            this.state = 31;
            this.match(VeDclParser.T__4);
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

EntryContext.prototype.control = function() {
    return this.getTypedRuleContext(ControlContext,0);
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
    this.enterRule(localctx, 4, VeDclParser.RULE_entry);
    try {
        this.state = 36;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,3,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 34;
            this.attribute();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 35;
            this.control();
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


function ControlContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeDclParser.RULE_control;
    return this;
}

ControlContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ControlContext.prototype.constructor = ControlContext;


 
ControlContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};


function ButtonContext(parser, ctx) {
	ControlContext.call(this, parser);
    ControlContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ButtonContext.prototype = Object.create(ControlContext.prototype);
ButtonContext.prototype.constructor = ButtonContext;

VeDclParser.ButtonContext = ButtonContext;

ButtonContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

ButtonContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
ButtonContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterButton(this);
	}
};

ButtonContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitButton(this);
	}
};


function EditBoxContext(parser, ctx) {
	ControlContext.call(this, parser);
    ControlContext.prototype.copyFrom.call(this, ctx);
    return this;
}

EditBoxContext.prototype = Object.create(ControlContext.prototype);
EditBoxContext.prototype.constructor = EditBoxContext;

VeDclParser.EditBoxContext = EditBoxContext;

EditBoxContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

EditBoxContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
EditBoxContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterEditBox(this);
	}
};

EditBoxContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitEditBox(this);
	}
};


function ColumnContext(parser, ctx) {
	ControlContext.call(this, parser);
    ControlContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ColumnContext.prototype = Object.create(ControlContext.prototype);
ColumnContext.prototype.constructor = ColumnContext;

VeDclParser.ColumnContext = ColumnContext;

ColumnContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

ColumnContext.prototype.entry = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(EntryContext);
    } else {
        return this.getTypedRuleContext(EntryContext,i);
    }
};
ColumnContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterColumn(this);
	}
};

ColumnContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitColumn(this);
	}
};


function RowContext(parser, ctx) {
	ControlContext.call(this, parser);
    ControlContext.prototype.copyFrom.call(this, ctx);
    return this;
}

RowContext.prototype = Object.create(ControlContext.prototype);
RowContext.prototype.constructor = RowContext;

VeDclParser.RowContext = RowContext;

RowContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

RowContext.prototype.entry = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(EntryContext);
    } else {
        return this.getTypedRuleContext(EntryContext,i);
    }
};
RowContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterRow(this);
	}
};

RowContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitRow(this);
	}
};


function TextContext(parser, ctx) {
	ControlContext.call(this, parser);
    ControlContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TextContext.prototype = Object.create(ControlContext.prototype);
TextContext.prototype.constructor = TextContext;

VeDclParser.TextContext = TextContext;

TextContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

TextContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};
TextContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterText(this);
	}
};

TextContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitText(this);
	}
};



VeDclParser.ControlContext = ControlContext;

VeDclParser.prototype.control = function() {

    var localctx = new ControlContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, VeDclParser.RULE_control);
    var _la = 0; // Token type
    try {
        this.state = 118;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,19,this._ctx);
        switch(la_) {
        case 1:
            localctx = new RowContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 39;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.ID) {
                this.state = 38;
                this.match(VeDclParser.ID);
            }

            this.state = 41;
            this.match(VeDclParser.T__0);
            this.state = 42;
            this.match(VeDclParser.T__5);
            this.state = 43;
            this.match(VeDclParser.T__2);
            this.state = 47;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
                this.state = 44;
                this.entry();
                this.state = 49;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 50;
            this.match(VeDclParser.T__3);
            this.state = 52;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__4) {
                this.state = 51;
                this.match(VeDclParser.T__4);
            }

            break;

        case 2:
            localctx = new ColumnContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 55;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.ID) {
                this.state = 54;
                this.match(VeDclParser.ID);
            }

            this.state = 57;
            this.match(VeDclParser.T__0);
            this.state = 58;
            this.match(VeDclParser.T__6);
            this.state = 59;
            this.match(VeDclParser.T__2);
            this.state = 63;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
                this.state = 60;
                this.entry();
                this.state = 65;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 66;
            this.match(VeDclParser.T__3);
            this.state = 68;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__4) {
                this.state = 67;
                this.match(VeDclParser.T__4);
            }

            break;

        case 3:
            localctx = new TextContext(this, localctx);
            this.enterOuterAlt(localctx, 3);
            this.state = 71;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.ID) {
                this.state = 70;
                this.match(VeDclParser.ID);
            }

            this.state = 73;
            this.match(VeDclParser.T__0);
            this.state = 74;
            this.match(VeDclParser.T__7);
            this.state = 75;
            this.match(VeDclParser.T__2);
            this.state = 79;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 76;
                this.attribute();
                this.state = 81;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 82;
            this.match(VeDclParser.T__3);
            this.state = 84;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__4) {
                this.state = 83;
                this.match(VeDclParser.T__4);
            }

            break;

        case 4:
            localctx = new ButtonContext(this, localctx);
            this.enterOuterAlt(localctx, 4);
            this.state = 87;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.ID) {
                this.state = 86;
                this.match(VeDclParser.ID);
            }

            this.state = 89;
            this.match(VeDclParser.T__0);
            this.state = 90;
            this.match(VeDclParser.T__8);
            this.state = 91;
            this.match(VeDclParser.T__2);
            this.state = 95;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 92;
                this.attribute();
                this.state = 97;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 98;
            this.match(VeDclParser.T__3);
            this.state = 100;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__4) {
                this.state = 99;
                this.match(VeDclParser.T__4);
            }

            break;

        case 5:
            localctx = new EditBoxContext(this, localctx);
            this.enterOuterAlt(localctx, 5);
            this.state = 103;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.ID) {
                this.state = 102;
                this.match(VeDclParser.ID);
            }

            this.state = 105;
            this.match(VeDclParser.T__0);
            this.state = 106;
            this.match(VeDclParser.T__9);
            this.state = 107;
            this.match(VeDclParser.T__2);
            this.state = 111;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeDclParser.ID) {
                this.state = 108;
                this.attribute();
                this.state = 113;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 114;
            this.match(VeDclParser.T__3);
            this.state = 116;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeDclParser.T__4) {
                this.state = 115;
                this.match(VeDclParser.T__4);
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
    this.enterRule(localctx, 8, VeDclParser.RULE_attribute);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 120;
        this.attributeName();
        this.state = 121;
        this.match(VeDclParser.T__10);
        this.state = 122;
        this.attributeValue();
        this.state = 123;
        this.match(VeDclParser.T__4);
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
    this.enterRule(localctx, 10, VeDclParser.RULE_attributeName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 125;
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
    this.enterRule(localctx, 12, VeDclParser.RULE_attributeValue);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 127;
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
