// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeDclListener = require('./VeDclListener').VeDclListener;
var grammarFileName = "VeDcl.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\r5\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004\t\u0004",
    "\u0004\u0005\t\u0005\u0003\u0002\u0007\u0002\f\n\u0002\f\u0002\u000e",
    "\u0002\u000f\u000b\u0002\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0007\u0003\u0017\n\u0003\f\u0003\u000e\u0003",
    "\u001a\u000b\u0003\u0003\u0003\u0003\u0003\u0005\u0003\u001e\n\u0003",
    "\u0003\u0004\u0005\u0004!\n\u0004\u0003\u0004\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0007\u0004\'\n\u0004\f\u0004\u000e\u0004*\u000b\u0004",
    "\u0003\u0004\u0003\u0004\u0005\u0004.\n\u0004\u0003\u0005\u0003\u0005",
    "\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0002\u0002\u0006\u0002",
    "\u0004\u0006\b\u0002\u0002\u00027\u0002\r\u0003\u0002\u0002\u0002\u0004",
    "\u0010\u0003\u0002\u0002\u0002\u0006 \u0003\u0002\u0002\u0002\b/\u0003",
    "\u0002\u0002\u0002\n\f\u0005\u0004\u0003\u0002\u000b\n\u0003\u0002\u0002",
    "\u0002\f\u000f\u0003\u0002\u0002\u0002\r\u000b\u0003\u0002\u0002\u0002",
    "\r\u000e\u0003\u0002\u0002\u0002\u000e\u0003\u0003\u0002\u0002\u0002",
    "\u000f\r\u0003\u0002\u0002\u0002\u0010\u0011\u0007\n\u0002\u0002\u0011",
    "\u0012\u0007\u0003\u0002\u0002\u0012\u0013\u0007\u0004\u0002\u0002\u0013",
    "\u0018\u0007\u0005\u0002\u0002\u0014\u0017\u0005\b\u0005\u0002\u0015",
    "\u0017\u0005\u0006\u0004\u0002\u0016\u0014\u0003\u0002\u0002\u0002\u0016",
    "\u0015\u0003\u0002\u0002\u0002\u0017\u001a\u0003\u0002\u0002\u0002\u0018",
    "\u0016\u0003\u0002\u0002\u0002\u0018\u0019\u0003\u0002\u0002\u0002\u0019",
    "\u001b\u0003\u0002\u0002\u0002\u001a\u0018\u0003\u0002\u0002\u0002\u001b",
    "\u001d\u0007\u0006\u0002\u0002\u001c\u001e\u0007\u0007\u0002\u0002\u001d",
    "\u001c\u0003\u0002\u0002\u0002\u001d\u001e\u0003\u0002\u0002\u0002\u001e",
    "\u0005\u0003\u0002\u0002\u0002\u001f!\u0007\n\u0002\u0002 \u001f\u0003",
    "\u0002\u0002\u0002 !\u0003\u0002\u0002\u0002!\"\u0003\u0002\u0002\u0002",
    "\"#\u0007\u0003\u0002\u0002#$\u0007\t\u0002\u0002$(\u0007\u0005\u0002",
    "\u0002%\'\u0005\b\u0005\u0002&%\u0003\u0002\u0002\u0002\'*\u0003\u0002",
    "\u0002\u0002(&\u0003\u0002\u0002\u0002()\u0003\u0002\u0002\u0002)+\u0003",
    "\u0002\u0002\u0002*(\u0003\u0002\u0002\u0002+-\u0007\u0006\u0002\u0002",
    ",.\u0007\u0007\u0002\u0002-,\u0003\u0002\u0002\u0002-.\u0003\u0002\u0002",
    "\u0002.\u0007\u0003\u0002\u0002\u0002/0\u0007\n\u0002\u000201\u0007",
    "\b\u0002\u000212\u0007\u000b\u0002\u000223\u0007\u0007\u0002\u00023",
    "\t\u0003\u0002\u0002\u0002\t\r\u0016\u0018\u001d (-"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "':'", "'dialog'", "'{'", "'}'", "';'", "'='" ];

var symbolicNames = [ null, null, null, null, null, null, null, "CONTROL_NAME", 
                      "ID", "STRING", "NEWLINE", "WHITESPACE" ];

var ruleNames =  [ "file", "dialog", "control", "attribute" ];

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
VeDclParser.CONTROL_NAME = 7;
VeDclParser.ID = 8;
VeDclParser.STRING = 9;
VeDclParser.NEWLINE = 10;
VeDclParser.WHITESPACE = 11;

VeDclParser.RULE_file = 0;
VeDclParser.RULE_dialog = 1;
VeDclParser.RULE_control = 2;
VeDclParser.RULE_attribute = 3;


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
        this.state = 11;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.ID) {
            this.state = 8;
            this.dialog();
            this.state = 13;
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

DialogContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};

DialogContext.prototype.control = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ControlContext);
    } else {
        return this.getTypedRuleContext(ControlContext,i);
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
        this.state = 14;
        this.match(VeDclParser.ID);
        this.state = 15;
        this.match(VeDclParser.T__0);
        this.state = 16;
        this.match(VeDclParser.T__1);
        this.state = 17;
        this.match(VeDclParser.T__2);
        this.state = 22;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.T__0 || _la===VeDclParser.ID) {
            this.state = 20;
            this._errHandler.sync(this);
            var la_ = this._interp.adaptivePredict(this._input,1,this._ctx);
            switch(la_) {
            case 1:
                this.state = 18;
                this.attribute();
                break;

            case 2:
                this.state = 19;
                this.control();
                break;

            }
            this.state = 24;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 25;
        this.match(VeDclParser.T__3);
        this.state = 27;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===VeDclParser.T__4) {
            this.state = 26;
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

ControlContext.prototype.CONTROL_NAME = function() {
    return this.getToken(VeDclParser.CONTROL_NAME, 0);
};

ControlContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

ControlContext.prototype.attribute = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AttributeContext);
    } else {
        return this.getTypedRuleContext(AttributeContext,i);
    }
};

ControlContext.prototype.enterRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.enterControl(this);
	}
};

ControlContext.prototype.exitRule = function(listener) {
    if(listener instanceof VeDclListener ) {
        listener.exitControl(this);
	}
};




VeDclParser.ControlContext = ControlContext;

VeDclParser.prototype.control = function() {

    var localctx = new ControlContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, VeDclParser.RULE_control);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 30;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===VeDclParser.ID) {
            this.state = 29;
            this.match(VeDclParser.ID);
        }

        this.state = 32;
        this.match(VeDclParser.T__0);
        this.state = 33;
        this.match(VeDclParser.CONTROL_NAME);
        this.state = 34;
        this.match(VeDclParser.T__2);
        this.state = 38;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===VeDclParser.ID) {
            this.state = 35;
            this.attribute();
            this.state = 40;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 41;
        this.match(VeDclParser.T__3);
        this.state = 43;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===VeDclParser.T__4) {
            this.state = 42;
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

AttributeContext.prototype.ID = function() {
    return this.getToken(VeDclParser.ID, 0);
};

AttributeContext.prototype.STRING = function() {
    return this.getToken(VeDclParser.STRING, 0);
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
    this.enterRule(localctx, 6, VeDclParser.RULE_attribute);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 45;
        this.match(VeDclParser.ID);
        this.state = 46;
        this.match(VeDclParser.T__5);
        this.state = 47;
        this.match(VeDclParser.STRING);
        this.state = 48;
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


exports.VeDclParser = VeDclParser;
