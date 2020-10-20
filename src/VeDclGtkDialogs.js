const fs = require('fs');
const antlr4 = require('antlr4');
const {VeDclLexer} = require('../grammar/VeDclLexer.js');
const {VeDclParser} = require('../grammar/VeDclParser.js');
const {VeDclListener} = require('../grammar/VeDclListener.js');

class VeDclGtkDialogs extends VeDclListener {
    constructor(context) {
        super();
        this.xml = '';
    }

    xml() {
        return this.xml;
    }

    enterFile(ctx) {
        this.xml = '<?xml version="1.0" encoding="UTF-8"?>';
        this.xml += '<interface>';        
    };

    exitFile(ctx) {
        this.xml += '</interface>';
    };

    enterDialog(ctx) {
    };

    exitDialog(ctx) {
    };

    enterControl(ctx) {
    };

    exitControl(ctx) {
    };

    enterAttribute(ctx) {
    };

    exitAttribute(ctx) {
    };

    /*
    visitTerminal(ctx) {
        const str = ctx.getText();
        // Also handles ID outside of expr
        //console.error('TERMINAL:', str);
        //console.error(ctx);
    }
    */

    getValue(expr) {
        if (expr instanceof Array) {
            return this.getValue(expr[0]);
        }
        return expr;
    }
}

function parse(dclfile) {
    const dcl = readFile(dclfile);
    const {tree} = parseDcl(dcl);
    const dialogs = new VeDclGtkDialogs();
    const walker = new antlr4.tree.ParseTreeWalker();
    walker.walk(dialogs, tree);
    return dialogs.xml;
}

function tree(dclfile) {
    const dcl = readFile(dclfile);
    const {parser, tree} = parseDcl(dcl);
    return tree.toStringTree(parser.ruleNames);
}

function readFile(file) {
    // TODO: handle errors
    return fs.readFileSync(file).toString();
}

function parseDcl(dcl) {
    const chars = new antlr4.InputStream(dcl);
    const lexer = new VeDclLexer(chars);
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new VeDclParser(tokens);
    //parser.removeErrorListeners();
    //parser.addErrorListener(new VeDclErrorListener());
    return {
        lexer,
        tokens,
        parser,
        tree: parser.file(),
    };
}

exports.parse = parse;
exports.tree = tree;
