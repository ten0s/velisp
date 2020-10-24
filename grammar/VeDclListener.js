// Generated from grammar/VeDcl.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete listener for a parse tree produced by VeDclParser.
function VeDclListener() {
	antlr4.tree.ParseTreeListener.call(this);
	return this;
}

VeDclListener.prototype = Object.create(antlr4.tree.ParseTreeListener.prototype);
VeDclListener.prototype.constructor = VeDclListener;

// Enter a parse tree produced by VeDclParser#file.
VeDclListener.prototype.enterFile = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#file.
VeDclListener.prototype.exitFile = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#dialog.
VeDclListener.prototype.enterDialog = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#dialog.
VeDclListener.prototype.exitDialog = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#entry.
VeDclListener.prototype.enterEntry = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#entry.
VeDclListener.prototype.exitEntry = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#text.
VeDclListener.prototype.enterText = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#text.
VeDclListener.prototype.exitText = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#button.
VeDclListener.prototype.enterButton = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#button.
VeDclListener.prototype.exitButton = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#editBox.
VeDclListener.prototype.enterEditBox = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#editBox.
VeDclListener.prototype.exitEditBox = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#attribute.
VeDclListener.prototype.enterAttribute = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#attribute.
VeDclListener.prototype.exitAttribute = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#attributeName.
VeDclListener.prototype.enterAttributeName = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#attributeName.
VeDclListener.prototype.exitAttributeName = function(ctx) {
};


// Enter a parse tree produced by VeDclParser#attributeValue.
VeDclListener.prototype.enterAttributeValue = function(ctx) {
};

// Exit a parse tree produced by VeDclParser#attributeValue.
VeDclListener.prototype.exitAttributeValue = function(ctx) {
};



exports.VeDclListener = VeDclListener;